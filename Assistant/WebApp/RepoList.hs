{- git-annex assistant webapp repository list
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes, CPP #-}

module Assistant.WebApp.RepoList where

import Assistant.WebApp.Common
import Assistant.DaemonStatus
import Assistant.WebApp.Notifications
import Assistant.WebApp.Utility
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Remote.List (remoteListRefresh)
import Annex.UUID (getUUID)
import Logs.Remote
import Logs.Trust
import Config
import Config.Cost
import qualified Git
#ifdef WITH_XMPP
#endif

import qualified Data.Map as M

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	repolist <- lift $ repoList $ RepoSelector
		{ onlyCloud = False
		, onlyConfigured = True
		, includeHere = False
		}
	let n = length repolist
	let numrepos = show n
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }

makeMiscRepositories :: Widget
makeMiscRepositories = $(widgetFile "configurators/repositories/misc")

makeCloudRepositories :: Bool -> Widget
makeCloudRepositories onlyTransfer = $(widgetFile "configurators/repositories/cloud")

{- Lists known repositories, followed by options to add more. -}
getRepositoriesR :: Handler RepHtml
getRepositoriesR = page "Repositories" (Just Repositories) $ do
	let repolist = repoListDisplay $ RepoSelector
		{ onlyCloud = False
		, onlyConfigured = False
		, includeHere = True
		}
	$(widgetFile "configurators/repositories")

data Actions
	= DisabledRepoActions
		{ setupRepoLink :: Route WebApp }
	| SyncingRepoActions
		{ setupRepoLink :: Route WebApp
		, syncToggleLink :: Route WebApp
		}
	| NotSyncingRepoActions
		{ setupRepoLink :: Route WebApp
		, syncToggleLink :: Route WebApp
		}

mkSyncingRepoActions :: UUID -> Actions
mkSyncingRepoActions u = SyncingRepoActions
	{ setupRepoLink = EditRepositoryR u
	, syncToggleLink = DisableSyncR u
	}

mkNotSyncingRepoActions :: UUID -> Actions
mkNotSyncingRepoActions u = NotSyncingRepoActions
	{ setupRepoLink = EditRepositoryR u
	, syncToggleLink = EnableSyncR u
	}

needsEnabled :: Actions -> Bool
needsEnabled (DisabledRepoActions _) = True
needsEnabled _ = False

notSyncing :: Actions -> Bool
notSyncing (SyncingRepoActions _ _) = False
notSyncing _ = True

{- Called by client to get a list of repos, that refreshes
 - when new repos as added.
 -
 - Returns a div, which will be inserted into the calling page.
 -}
getRepoListR :: RepoListNotificationId -> Handler RepHtml
getRepoListR (RepoListNotificationId nid reposelector) = do
	waitNotifier getRepoListBroadcaster nid
	p <- widgetToPageContent $ repoListDisplay reposelector
	hamletToRepHtml $ [hamlet|^{pageBody p}|]

repoListDisplay :: RepoSelector -> Widget
repoListDisplay reposelector = do
	autoUpdate ident (NotifierRepoListR reposelector) (10 :: Int) (10 :: Int)
	addScript $ StaticR jquery_ui_core_js
	addScript $ StaticR jquery_ui_widget_js
	addScript $ StaticR jquery_ui_mouse_js
	addScript $ StaticR jquery_ui_sortable_js

	repolist <- lift $ repoList reposelector

	$(widgetFile "configurators/repositories/list")

  where
	ident = "repolist"

-- (num, name, (uuid, actions))
type RepoList = [(String, String, (UUID, Actions))]

{- A numbered list of known repositories,
 - with actions that can be taken on them. -}
repoList :: RepoSelector -> Handler RepoList
repoList reposelector
	| onlyConfigured reposelector = list =<< configured
	| otherwise = list =<< (++) <$> configured <*> rest
  where
	configured = do
		rs <- filter wantedrepo . syncRemotes
			<$> liftAssistant getDaemonStatus
		liftAnnex $ do
			let us = map Remote.uuid rs
			let l = zip us $ map mkSyncingRepoActions us
			if includeHere reposelector
				then do
					u <- getUUID
					autocommit <- annexAutoCommit <$> Annex.getGitConfig
					let hereactions = if autocommit
						then mkSyncingRepoActions u
						else mkNotSyncingRepoActions u
					let here = (u, hereactions)
					return $ here : l
				else return l
	rest = liftAnnex $ do
		m <- readRemoteLog
		unconfigured <- map snd . catMaybes . filter wantedremote 
			. map (findinfo m)
			<$> (trustExclude DeadTrusted $ M.keys m)
		unsyncable <- map Remote.uuid . filter wantedrepo .
			filter (not . remoteAnnexSync . Remote.gitconfig)
			<$> Remote.enabledRemoteList
		return $ zip unsyncable (map mkNotSyncingRepoActions unsyncable) ++ unconfigured
	wantedrepo r
		| Remote.readonly r = False
		| onlyCloud reposelector = Git.repoIsUrl (Remote.repo r) && not (isXMPPRemote r)
		| otherwise = True
	wantedremote Nothing = False
	wantedremote (Just (iscloud, _))
		| onlyCloud reposelector = iscloud
		| otherwise = True
	findinfo m u = case M.lookup u m of
		Nothing -> Nothing
		Just c -> case M.lookup "type" c of
			Just "rsync" -> val True EnableRsyncR
			Just "directory" -> val False EnableDirectoryR
#ifdef WITH_S3
			Just "S3" -> val True EnableS3R
#endif
			Just "glacier" -> val True EnableGlacierR
#ifdef WITH_WEBDAV
			Just "webdav" -> val True EnableWebDAVR
#endif
			_ -> Nothing
	  where
		val iscloud r = Just (iscloud, (u, DisabledRepoActions $ r u))
	list l = liftAnnex $ do
		let l' = nubBy (\x y -> fst x == fst y) l
		zip3
			<$> pure counter
			<*> Remote.prettyListUUIDs (map fst l')
			<*> pure l'
	counter = map show ([1..] :: [Int])

getEnableSyncR :: UUID -> Handler ()
getEnableSyncR = flipSync True

getDisableSyncR :: UUID -> Handler ()
getDisableSyncR = flipSync False

flipSync :: Bool -> UUID -> Handler ()
flipSync enable uuid = do
	mremote <- liftAnnex $ Remote.remoteFromUUID uuid
	changeSyncable mremote enable
	redirect RepositoriesR

getRepositoriesReorderR :: Handler ()
getRepositoriesReorderR = do
	moved <- runInputGet $ ireq textField "moved"
	list <- lookupGetParams "list[]"
	error $ show (moved, list)

reorderRepository :: UUID -> Handler ()
reorderRepository uuid = do
	void $ liftAnnex $ do
		remote <- fromMaybe (error "Unknown UUID") <$>
			Remote.remoteFromUUID uuid
		rs <- Remote.enabledRemoteList
		let us = map Remote.uuid rs
		case afteruuid us >>= (\u -> elemIndex u us) of
			Nothing -> noop -- already at end
			Just i -> do
				let rs' = filter other rs
				let costs = map Remote.cost rs'
				let rs'' = (\(x, y) -> x ++ [remote] ++ y) $
					splitAt (i + 1) rs'
				let l = zip rs'' (insertCostAfter costs i)
				forM_ l $ \(r, newcost) ->
					when (Remote.cost r /= newcost) $
						setRemoteCost r newcost
		remoteListRefresh
	liftAssistant updateSyncRemotes
  where
  	afteruuid [] = Nothing
	afteruuid (u:us)
		| u == uuid = headMaybe us
		| otherwise = afteruuid us
	other r = Remote.uuid r /= uuid
