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
import Assistant.WebApp.Utility
import Utility.Yesod
import qualified Remote
import Logs.UUID
import Logs.Group
import Logs.PreferredContent
import Types.StandardGroups
import qualified Config
import Annex.UUID
import qualified Git

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
	, repoSyncable :: Bool
	}
	deriving (Show)

getRepoConfig :: UUID -> Git.Repo -> Annex RepoConfig
getRepoConfig uuid r = RepoConfig
	<$> (T.pack . fromMaybe "" . M.lookup uuid <$> uuidMap)
	<*> getrepogroup
	<*> Config.repoSyncable r
	where
		getrepogroup = do
			groups <- lookupGroups uuid
			return $ 
				maybe (RepoGroupCustom $ unwords $ S.toList groups) RepoGroupStandard
					(getStandardGroup groups)

{- Returns Just False if syncing should be disabled, Just True when enabled;
 - Nothing when it is not changed. -}
setRepoConfig :: UUID -> Git.Repo -> RepoConfig -> Annex (Maybe Bool)
setRepoConfig uuid r c = do
	describeUUID uuid $ T.unpack $ repoDescription c
	case repoGroup c of
		RepoGroupStandard g -> setStandardGroup uuid g
		RepoGroupCustom s -> groupSet uuid $ S.fromList $ words s
	ifM ((==) uuid <$> getUUID)
		( return Nothing
		, do
			syncable <- Config.repoSyncable r
			return $ if (syncable /= repoSyncable c)
				then Just $ repoSyncable c
				else Nothing
		)

editRepositoryAForm :: RepoConfig -> AForm WebApp WebApp RepoConfig
editRepositoryAForm def = RepoConfig
	<$> areq textField "Description" (Just $ repoDescription def)
	<*> areq (selectFieldList $ customgroups++standardgroups) "Repository group" (Just $ repoGroup def)
	<*> areq checkBoxField "Syncing enabled" (Just $ repoSyncable def)
	where
		standardgroups :: [(Text, RepoGroup)]
		standardgroups = map (\g -> (T.pack $ descStandardGroup g , RepoGroupStandard g))
			[minBound :: StandardGroup .. maxBound :: StandardGroup]
		customgroups :: [(Text, RepoGroup)]
		customgroups = case repoGroup def of
			RepoGroupCustom s -> [(T.pack s, RepoGroupCustom s)]
			_ -> []

getEditRepositoryR :: UUID -> Handler RepHtml
getEditRepositoryR = editForm False

getEditNewRepositoryR :: UUID -> Handler RepHtml
getEditNewRepositoryR = editForm True

editForm :: Bool -> UUID -> Handler RepHtml
editForm new uuid = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Configure repository"

	(repo, mremote) <- lift $ runAnnex undefined $ Remote.repoFromUUID uuid
	curr <- lift $ runAnnex undefined $ getRepoConfig uuid repo
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ editRepositoryAForm curr
	case result of
		FormSuccess input -> lift $ do
			syncchanged <- runAnnex undefined $
				setRepoConfig uuid repo input
			maybe noop (changeSyncable mremote) syncchanged
			redirect RepositoriesR
		_ -> showform form enctype curr
	where
		showform form enctype curr = do
			let istransfer = repoGroup curr == RepoGroupStandard TransferGroup
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/editrepository")
