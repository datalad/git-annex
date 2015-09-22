{- git-annex assistant webapp configurators for Amazon AWS services
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.AWS where

import Assistant.WebApp.Common
import Assistant.WebApp.MakeRemote
#ifdef WITH_S3
import qualified Remote.S3 as S3
#endif
import qualified Remote.Glacier as Glacier
import qualified Remote.Helper.AWS as AWS
import Logs.Remote
import qualified Remote
import qualified Types.Remote as Remote
import Types.Remote (RemoteConfig)
import Types.StandardGroups
import Creds
import Assistant.Gpg
import Git.Types (RemoteName)

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

awsConfigurator :: Widget -> Handler Html
awsConfigurator = page "Add an Amazon repository" (Just Configuration)

glacierConfigurator :: Widget -> Handler Html
glacierConfigurator a = do
	ifM (liftIO $ inPath "glacier")
		( awsConfigurator a
		, awsConfigurator needglaciercli
		)
  where
	needglaciercli = $(widgetFile "configurators/needglaciercli")

data StorageClass = StandardRedundancy | StandardInfrequentAccess | ReducedRedundancy
	deriving (Eq, Enum, Bounded)

instance Show StorageClass where
	show StandardRedundancy = "STANDARD" 
	show StandardInfrequentAccess = "STANDARD_IA"
	show ReducedRedundancy = "REDUCED_REDUNDANCY"

data AWSInput = AWSInput
	{ accessKeyID :: Text
	, secretAccessKey :: Text
	, datacenter :: Text
	-- Only used for S3, not Glacier.
	, storageClass :: StorageClass
	, repoName :: Text
	, enableEncryption :: EnableEncryption
	}

data AWSCreds = AWSCreds Text Text

extractCreds :: AWSInput -> AWSCreds
extractCreds i = AWSCreds (accessKeyID i) (secretAccessKey i)

s3InputAForm :: Maybe CredPair -> MkAForm AWSInput
s3InputAForm defcreds = AWSInput
	<$> accessKeyIDFieldWithHelp (T.pack . fst <$> defcreds)
	<*> secretAccessKeyField (T.pack . snd <$> defcreds)
	<*> datacenterField AWS.S3
	<*> areq (selectFieldList storageclasses) (bfs "Storage class") (Just StandardRedundancy)
	<*> areq textField (bfs "Repository name") (Just "S3")
	<*> enableEncryptionField
  where
	storageclasses :: [(Text, StorageClass)]
	storageclasses =
		[ ("Standard redundancy", StandardRedundancy)
#if MIN_VERSION_aws(0,13,0)
		, ("Infrequent access (cheaper for backups and archives)", StandardInfrequentAccess)
#endif
		, ("Reduced redundancy (costs less)", ReducedRedundancy)
		]

glacierInputAForm :: Maybe CredPair -> MkAForm AWSInput
glacierInputAForm defcreds = AWSInput
	<$> accessKeyIDFieldWithHelp (T.pack . fst <$> defcreds)
	<*> secretAccessKeyField (T.pack . snd <$> defcreds)
	<*> datacenterField AWS.Glacier
	<*> pure StandardRedundancy
	<*> areq textField (bfs "Repository name") (Just "glacier")
	<*> enableEncryptionField

awsCredsAForm :: Maybe CredPair -> MkAForm AWSCreds
awsCredsAForm defcreds = AWSCreds
	<$> accessKeyIDFieldWithHelp (T.pack . fst <$> defcreds)
	<*> secretAccessKeyField (T.pack . snd <$> defcreds)

accessKeyIDField :: Widget -> Maybe Text -> MkAForm Text
accessKeyIDField help = areq (textField `withNote` help) (bfs "Access Key ID")

accessKeyIDFieldWithHelp :: Maybe Text -> MkAForm Text
accessKeyIDFieldWithHelp = accessKeyIDField help
  where
	help = [whamlet|
<a href="https://portal.aws.amazon.com/gp/aws/securityCredentials#id_block">
  Get Amazon access keys
|]

secretAccessKeyField :: Maybe Text -> MkAForm Text
secretAccessKeyField = areq passwordField (bfs "Secret Access Key")

datacenterField :: AWS.Service -> MkAForm Text
datacenterField service = areq (selectFieldList list) (bfs "Datacenter") defregion
  where
	list = M.toList $ AWS.regionMap service
	defregion = Just $ AWS.defaultRegion service

getAddS3R :: Handler Html
getAddS3R = postAddS3R

postAddS3R :: Handler Html
#ifdef WITH_S3
postAddS3R = awsConfigurator $ do
	defcreds <- liftAnnex previouslyUsedAWSCreds
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ s3InputAForm defcreds
	case result of
		FormSuccess input -> liftH $ do
			let name = T.unpack $ repoName input
			makeAWSRemote initSpecialRemote S3.remote TransferGroup (extractCreds input) name $ M.fromList
				[ configureEncryption $ enableEncryption input
				, ("type", "S3")
				, ("datacenter", T.unpack $ datacenter input)
				, ("storageclass", show $ storageClass input)
				, ("chunk", "1MiB")
				]
		_ -> $(widgetFile "configurators/adds3")
#else
postAddS3R = error "S3 not supported by this build"
#endif

getAddGlacierR :: Handler Html
getAddGlacierR = postAddGlacierR

postAddGlacierR :: Handler Html
#ifdef WITH_S3
postAddGlacierR = glacierConfigurator $ do
	defcreds <- liftAnnex previouslyUsedAWSCreds
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ glacierInputAForm defcreds
	case result of
		FormSuccess input -> liftH $ do
			let name = T.unpack $ repoName input
			makeAWSRemote initSpecialRemote Glacier.remote SmallArchiveGroup (extractCreds input) name $ M.fromList
				[ configureEncryption $ enableEncryption input
				, ("type", "glacier")
				, ("datacenter", T.unpack $ datacenter input)
				]
		_ -> $(widgetFile "configurators/addglacier")
#else
postAddGlacierR = error "S3 not supported by this build"
#endif

getEnableS3R :: UUID -> Handler Html
#ifdef WITH_S3
getEnableS3R uuid = do
	m <- liftAnnex readRemoteLog
	if maybe False S3.configIA (M.lookup uuid m)
		then redirect $ EnableIAR uuid
		else postEnableS3R uuid
#else
getEnableS3R = postEnableS3R
#endif

postEnableS3R :: UUID -> Handler Html
#ifdef WITH_S3
postEnableS3R uuid = awsConfigurator $ enableAWSRemote S3.remote uuid
#else
postEnableS3R _ = error "S3 not supported by this build"
#endif

getEnableGlacierR :: UUID -> Handler Html
getEnableGlacierR = postEnableGlacierR

postEnableGlacierR :: UUID -> Handler Html
postEnableGlacierR = glacierConfigurator . enableAWSRemote Glacier.remote

enableAWSRemote :: RemoteType -> UUID -> Widget
#ifdef WITH_S3
enableAWSRemote remotetype uuid = do
	defcreds <- liftAnnex previouslyUsedAWSCreds
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ awsCredsAForm defcreds
	case result of
		FormSuccess creds -> liftH $ do
			m <- liftAnnex readRemoteLog
			let name = fromJust $ M.lookup "name" $
				fromJust $ M.lookup uuid m
			makeAWSRemote enableSpecialRemote remotetype SmallArchiveGroup creds name M.empty
		_ -> do
			description <- liftAnnex $
				T.pack <$> Remote.prettyUUID uuid
			$(widgetFile "configurators/enableaws")
#else
enableAWSRemote _ _ = error "S3 not supported by this build"
#endif

makeAWSRemote :: SpecialRemoteMaker -> RemoteType -> StandardGroup -> AWSCreds -> RemoteName -> RemoteConfig -> Handler ()
makeAWSRemote maker remotetype defaultgroup (AWSCreds ak sk) name config = 
	setupCloudRemote defaultgroup Nothing $
		maker hostname remotetype (Just creds) config
  where
	creds = (T.unpack ak, T.unpack sk)
	{- AWS services use the remote name as the basis for a host
	 - name, so filter it to contain valid characters. -}
	hostname = case filter isAlphaNum name of
		[] -> "aws"
		n -> n

getRepoInfo :: RemoteConfig -> Widget
getRepoInfo c = [whamlet|S3 remote using bucket: #{bucket}|]
  where
	bucket = fromMaybe "" $ M.lookup "bucket" c

#ifdef WITH_S3
previouslyUsedAWSCreds :: Annex (Maybe CredPair)
previouslyUsedAWSCreds = getM gettype [S3.remote, Glacier.remote]
  where
	gettype t = previouslyUsedCredPair AWS.creds t $
		not . S3.configIA . Remote.config
#endif
