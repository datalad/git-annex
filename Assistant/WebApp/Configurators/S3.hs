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

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

data S3Input = S3Input
	{ accessKeyID :: Text
	, secretAccessKey :: Text
	-- Free form text for datacenter because Amazon adds new ones.
	, datacenter :: Text
	, storageClass :: StorageClass
	, repoName :: Text
	}
	deriving (Show)

data StorageClass = StandardRedundancy | ReducedRedundancy
	deriving (Eq, Enum, Bounded)

instance Show StorageClass where
	show StandardRedundancy = "STANDARD" 
	show ReducedRedundancy = "REDUCED_REDUNDANCY"

s3AForm :: AForm WebApp WebApp S3Input
s3AForm = S3Input
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

getAddS3R :: Handler RepHtml
getAddS3R = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add an Amazon S3 repository"
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap s3AForm
	case result of
		FormSuccess s3input -> lift $ do
			let name = T.unpack $ repoName s3input
			name' <- runAnnex name $
				fromRepo $ uniqueRemoteName name 0
			makeS3Remote s3input name'
		_ -> showform form enctype
	where
		showform form enctype = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/adds3")

makeS3Remote :: S3Input -> String -> Handler ()
makeS3Remote s3input name = do
	webapp <- getYesod
	let st = fromJust $ threadState webapp
	liftIO $ do
		S3.s3SetCredsEnv
			( T.unpack $ accessKeyID s3input
			, T.unpack $ secretAccessKey s3input
			)
		r <- runThreadState st $ addRemote $
			makeSpecialRemote name S3.remote config
		syncNewRemote st (daemonStatus webapp) (scanRemotes webapp) r
	redirect RepositoriesR
	where
		config = M.fromList
			[ ("encryption", "shared")
			, ("type", "S3")
			, ("datacenter", T.unpack $ datacenter s3input)
			, ("storageclass", show $ storageClass s3input)
			]
