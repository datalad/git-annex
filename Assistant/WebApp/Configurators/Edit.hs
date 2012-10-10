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
import Logs.Group
import Types.StandardGroups

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

data RepoGroup = RepoGroupCustom String | RepoGroupStandard StandardGroup
	deriving (Show, Eq)

data RepoConfig = RepoConfig
	{ repoDescription :: Text
	, repoGroup :: RepoGroup
	}
	deriving (Show)

editRepositoryAForm :: RepoConfig -> AForm WebApp WebApp RepoConfig
editRepositoryAForm def = RepoConfig
	<$> areq textField "Description" (Just $ repoDescription def)
	<*> areq (selectFieldList $ customgroups++standardgroups) "Repository group" (Just $ repoGroup def)
	where
		standardgroups :: [(Text, RepoGroup)]
		standardgroups = map (\g -> (T.pack $ descStandardGroup g , RepoGroupStandard g))
			[minBound :: StandardGroup .. maxBound :: StandardGroup]
		customgroups :: [(Text, RepoGroup)]
		customgroups = case repoGroup def of
			RepoGroupCustom s -> [(T.pack s, RepoGroupCustom s)]
			_ -> []

getRepoConfig :: UUID -> Annex RepoConfig
getRepoConfig uuid = RepoConfig
	<$> (T.pack . fromMaybe "" . M.lookup uuid <$> uuidMap)
	<*> getrepogroup
	where
		getrepogroup = do
			groups <- lookupGroups uuid
			return $ 
				maybe (RepoGroupCustom $ unwords $ S.toList groups) RepoGroupStandard
					(getStandardGroup groups)

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
