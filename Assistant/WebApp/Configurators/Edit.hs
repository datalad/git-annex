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
import Assistant.DaemonStatus
import Assistant.MakeRemote (uniqueRemoteName)
import Assistant.WebApp.Configurators.XMPP (xmppNeeded)
import Utility.Yesod
import qualified Remote
import qualified Remote.List as Remote
import Logs.UUID
import Logs.Group
import Logs.PreferredContent
import Types.StandardGroups
import qualified Config
import qualified Git
import qualified Git.Command

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

data RepoGroup = RepoGroupCustom String | RepoGroupStandard StandardGroup
	deriving (Show, Eq)

data RepoConfig = RepoConfig
	{ repoName :: Text
	, repoDescription :: Maybe Text
	, repoGroup :: RepoGroup
	, repoSyncable :: Bool
	}
	deriving (Show)

getRepoConfig :: UUID -> Git.Repo -> Maybe Remote -> Annex RepoConfig
getRepoConfig uuid r mremote = RepoConfig
	<$> pure (T.pack $ maybe "here" Remote.name mremote)
	<*> (maybe Nothing (Just . T.pack) . M.lookup uuid <$> uuidMap)
	<*> getrepogroup
	<*> Config.repoSyncable r
	where
		getrepogroup = do
			groups <- lookupGroups uuid
			return $ 
				maybe (RepoGroupCustom $ unwords $ S.toList groups) RepoGroupStandard
					(getStandardGroup groups)

setRepoConfig :: UUID -> Maybe Remote -> RepoConfig -> RepoConfig -> Handler ()
setRepoConfig uuid mremote oldc newc = do
	when (repoDescription oldc /= repoDescription newc) $ runAnnex undefined $
		maybe noop (describeUUID uuid . T.unpack) (repoDescription newc)
	when (repoGroup oldc /= repoGroup newc) $ runAnnex undefined $ 
		case repoGroup newc of
			RepoGroupStandard g -> setStandardGroup uuid g
			RepoGroupCustom s -> groupSet uuid $ S.fromList $ words s
	when (repoSyncable oldc /= repoSyncable newc) $
		changeSyncable mremote (repoSyncable newc)
	when (isJust mremote && repoName oldc /= repoName newc) $ do
		runAnnex undefined $ do
			name <- fromRepo $ uniqueRemoteName (T.unpack $ repoName newc) 0
			inRepo $ Git.Command.run "remote"
				[ Param "rename"
				, Param $ T.unpack $ repoName oldc
				, Param name
				]
			void $ Remote.remoteListRefresh
		runAssistantY updateSyncRemotes

editRepositoryAForm :: RepoConfig -> AForm WebApp WebApp RepoConfig
editRepositoryAForm def = RepoConfig
	<$> areq textField "Name" (Just $ repoName def)
	<*> aopt textField "Description" (Just $ repoDescription def)
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

getEditNewCloudRepositoryR :: UUID -> Handler RepHtml
getEditNewCloudRepositoryR uuid = xmppNeeded >> editForm True uuid

editForm :: Bool -> UUID -> Handler RepHtml
editForm new uuid = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Configure repository"

	(repo, mremote) <- lift $ runAnnex undefined $ Remote.repoFromUUID uuid
	curr <- lift $ runAnnex undefined $ getRepoConfig uuid repo mremote
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ editRepositoryAForm curr
	case result of
		FormSuccess input -> lift $ do
			setRepoConfig uuid mremote curr input
			redirect RepositoriesR
		_ -> showform form enctype curr
	where
		showform form enctype curr = do
			let istransfer = repoGroup curr == RepoGroupStandard TransferGroup
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/editrepository")
