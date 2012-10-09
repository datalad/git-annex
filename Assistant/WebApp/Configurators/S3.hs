{- git-annex assistant webapp configurator for Amazon S3
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.S3 where

import Assistant.Common
import Assistant.MakeRemote
import Assistant.Sync
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Assistant.ThreadedMonad
import Utility.Yesod
import qualified Remote.S3 as S3
import Logs.Remote
import qualified Remote
import Types.Remote (RemoteConfig)
import Logs.Group

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

s3Configurator :: Widget -> Handler RepHtml
s3Configurator a = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add an Amazon S3 repository"
	a

data StorageClass = StandardRedundancy | ReducedRedundancy
	deriving (Eq, Enum, Bounded)

instance Show StorageClass where
	show StandardRedundancy = "STANDARD" 
	show ReducedRedundancy = "REDUCED_REDUNDANCY"

data S3Input = S3Input
	{ accessKeyID :: Text
	, secretAccessKey :: Text
	-- Free form text for datacenter because Amazon adds new ones.
	, datacenter :: Text
	, storageClass :: StorageClass
	, repoName :: Text
	}

data S3Creds = S3Creds Text Text

extractCreds :: S3Input -> S3Creds
extractCreds i = S3Creds (accessKeyID i) (secretAccessKey i)

s3InputAForm :: AForm WebApp WebApp S3Input
s3InputAForm = S3Input
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

s3CredsAForm :: AForm WebApp WebApp S3Creds
s3CredsAForm = S3Creds
	<$> areq textField "Access Key ID" Nothing
	<*> areq passwordField "Secret Access Key" Nothing

getAddS3R :: Handler RepHtml
getAddS3R = s3Configurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap s3InputAForm
	case result of
		FormSuccess s3input -> lift $ do
			let name = T.unpack $ repoName s3input
			makeS3Remote (extractCreds s3input) name setgroup $ M.fromList
				[ ("encryption", "shared")
				, ("type", "S3")
				, ("datacenter", T.unpack $ datacenter s3input)
				, ("storageclass", show $ storageClass s3input)
				]
		_ -> showform form enctype
	where
		showform form enctype = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/adds3")
		setgroup r = runAnnex () $
			groupSet (Remote.uuid r) (S.singleton "servers")

getEnableS3R :: UUID -> Handler RepHtml
getEnableS3R uuid = s3Configurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap s3CredsAForm
	case result of
		FormSuccess s3creds -> lift $ do
			m <- runAnnex M.empty readRemoteLog
			let name = fromJust $ M.lookup "name" $
				fromJust $ M.lookup uuid m
			makeS3Remote s3creds name (const noop) M.empty
		_ -> showform form enctype
	where
		showform form enctype = do
			let authtoken = webAppFormAuthToken
			description <- lift $ runAnnex "" $
				T.pack . concat <$> Remote.prettyListUUIDs [uuid]
			$(widgetFile "configurators/enables3")

makeS3Remote :: S3Creds -> String -> (Remote -> Handler ()) -> RemoteConfig -> Handler ()
makeS3Remote (S3Creds ak sk) name setup config = do
	webapp <- getYesod
	let st = fromJust $ threadState webapp
	remotename <- runAnnex name $ fromRepo $ uniqueRemoteName name 0
	liftIO $ S3.s3SetCredsEnv ( T.unpack ak, T.unpack sk)
	r <- liftIO $ runThreadState st $ addRemote $ do
		makeSpecialRemote name S3.remote config
		return remotename
	setup r
	liftIO $ syncNewRemote st (daemonStatus webapp) (scanRemotes webapp) r
	redirect RepositoriesR
