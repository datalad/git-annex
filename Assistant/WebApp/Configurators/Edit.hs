{- git-annex assistant webapp configurator for editing existing repos
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Edit where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod
import qualified Remote
import Logs.UUID

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

data RepoConfig = RepoConfig
	{ repoDescription :: Text
	}
	deriving (Show)

editRepositoryAForm :: RepoConfig -> AForm WebApp WebApp RepoConfig
editRepositoryAForm def = RepoConfig
	<$> areq textField "Description" (Just $ repoDescription def)

getRepoConfig :: UUID -> Annex RepoConfig
getRepoConfig uuid = RepoConfig
	<$> (T.pack . fromMaybe "" . M.lookup uuid <$> uuidMap)

getEditRepositoryR :: UUID -> Handler RepHtml
getEditRepositoryR uuid = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Configure repository"
	
	curr <- lift $ runAnnex undefined $ getRepoConfig uuid
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ editRepositoryAForm curr
	case result of
		FormSuccess input -> do
			error (show input)
		_ -> showform form enctype
	where
		showform form enctype = do
			let authtoken = webAppFormAuthToken
			description <- lift $
				runAnnex T.empty $  T.pack . concat <$>
					Remote.prettyListUUIDs [uuid]
			$(widgetFile "configurators/editrepository")
