{- git-annex assistant webapp configurators for Amazon AWS services
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.AWS where

import Assistant.WebApp.Common
import Assistant.MakeRemote
import Assistant.Sync
#ifdef WITH_S3
import qualified Remote.S3 as S3
#endif
import qualified Remote.Glacier as Glacier
import qualified Remote.Helper.AWS as AWS
import Logs.Remote
import qualified Remote
import Types.Remote (RemoteConfig)
import Types.StandardGroups
import Logs.PreferredContent

import Yesod
import qualified Data.Text as T
import qualified Data.Map as M

awsConfigurator :: Widget -> Handler RepHtml
awsConfigurator = page "Add an Amazon repository" (Just Config)

glacierConfigurator :: Widget -> Handler RepHtml
glacierConfigurator a = do
	ifM (liftIO $ inPath "glacier")
		( awsConfigurator a
		, awsConfigurator needglaciercli
		)
  where
	needglaciercli = $(widgetFile "configurators/needglaciercli")

data StorageClass = StandardRedundancy | ReducedRedundancy
	deriving (Eq, Enum, Bounded)

instance Show StorageClass where
	show StandardRedundancy = "STANDARD" 
	show ReducedRedundancy = "REDUCED_REDUNDANCY"

data AWSInput = AWSInput
	{ accessKeyID :: Text
	, secretAccessKey :: Text
	-- Free form text for datacenter because Amazon adds new ones.
	, datacenter :: Text
	-- Only used for S3, not Glacier.
	, storageClass :: StorageClass
	, repoName :: Text
	}

data AWSCreds = AWSCreds Text Text

extractCreds :: AWSInput -> AWSCreds
extractCreds i = AWSCreds (accessKeyID i) (secretAccessKey i)

s3InputAForm :: AForm WebApp WebApp AWSInput
s3InputAForm = AWSInput
	<$> areq textField "Access Key ID" Nothing
	<*> areq passwordField "Secret Access Key" Nothing
	<*> areq textField "Datacenter" (Just "US")
	<*> areq (selectFieldList storageclasses) "Storage class" (Just StandardRedundancy)
	<*> areq textField "Repository name" (Just "S3")
  where
	storageclasses :: [(Text, StorageClass)]
	storageclasses =
		[ ("Standard redundancy", StandardRedundancy)
		, ("Reduced redundancy (costs less)", ReducedRedundancy)
		]

glacierInputAForm :: AForm WebApp WebApp AWSInput
glacierInputAForm = AWSInput
	<$> areq textField "Access Key ID" Nothing
	<*> areq passwordField "Secret Access Key" Nothing
	<*> areq textField "Datacenter" (Just "us-east-1")
	<*> pure StandardRedundancy
	<*> areq textField "Repository name" (Just "glacier")

awsCredsAForm :: AForm WebApp WebApp AWSCreds
awsCredsAForm = AWSCreds
	<$> areq textField "Access Key ID" Nothing
	<*> areq passwordField "Secret Access Key" Nothing

getAddS3R :: Handler RepHtml
#ifdef WITH_S3
getAddS3R = awsConfigurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap s3InputAForm
	case result of
		FormSuccess input -> lift $ do
			let name = T.unpack $ repoName input
			makeAWSRemote S3.remote (extractCreds input) name setgroup $ M.fromList
				[ ("encryption", "shared")
				, ("type", "S3")
				, ("datacenter", T.unpack $ datacenter input)
				, ("storageclass", show $ storageClass input)
				]
		_ -> $(widgetFile "configurators/adds3")
  where
	setgroup r = runAnnex () $
		setStandardGroup (Remote.uuid r) TransferGroup
#else
getAddS3R = error "S3 not supported by this build"
#endif

getAddGlacierR :: Handler RepHtml
getAddGlacierR = glacierConfigurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap glacierInputAForm
	case result of
		FormSuccess input -> lift $ do
			let name = T.unpack $ repoName input
			makeAWSRemote Glacier.remote (extractCreds input) name setgroup $ M.fromList
				[ ("encryption", "shared")
				, ("type", "glacier")
				, ("datacenter", T.unpack $ datacenter input)
				]
		_ -> $(widgetFile "configurators/addglacier")
  where
	setgroup r = runAnnex () $
		setStandardGroup (Remote.uuid r) SmallArchiveGroup

getEnableS3R :: UUID -> Handler RepHtml
#ifdef WITH_S3
getEnableS3R = awsConfigurator . enableAWSRemote S3.remote
#else
getEnableS3R _ = error "S3 not supported by this build"
#endif

getEnableGlacierR :: UUID -> Handler RepHtml
getEnableGlacierR = glacierConfigurator . enableAWSRemote Glacier.remote

enableAWSRemote :: RemoteType -> UUID -> Widget
enableAWSRemote remotetype uuid = do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap awsCredsAForm
	case result of
		FormSuccess creds -> lift $ do
			m <- runAnnex M.empty readRemoteLog
			let name = fromJust $ M.lookup "name" $
				fromJust $ M.lookup uuid m
			makeAWSRemote remotetype creds name (const noop) M.empty
		_ -> do
			description <- lift $ runAnnex "" $
				T.pack . concat <$> Remote.prettyListUUIDs [uuid]
			$(widgetFile "configurators/enableaws")

makeAWSRemote :: RemoteType -> AWSCreds -> String -> (Remote -> Handler ()) -> RemoteConfig -> Handler ()
makeAWSRemote remotetype (AWSCreds ak sk) name setup config = do
	remotename <- runAnnex name $ fromRepo $ uniqueRemoteName name 0
	liftIO $ AWS.setCredsEnv (T.unpack ak, T.unpack sk)
	r <- liftAssistant $ liftAnnex $ addRemote $ do
		makeSpecialRemote name remotetype config
		return remotename
	setup r
	liftAssistant $ syncNewRemote r
	redirect $ EditNewCloudRepositoryR $ Remote.uuid r
