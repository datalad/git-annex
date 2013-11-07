{- git-annex assistant webapp repository list
 -
 - Copyright 2012,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, CPP #-}

module Assistant.WebApp.RepoList where

import Assistant.WebApp.Common
import Assistant.DaemonStatus
import Assistant.WebApp.Notifications
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Remote.List (remoteListRefresh)
import Annex.UUID (getUUID)
import Logs.Remote
import Logs.Trust
import Logs.Group
import Config
import Git.Remote
import Assistant.Sync
import Config.Cost
import Utility.NotificationBroadcaster
import qualified Git

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Function

type RepoList = [(RepoDesc, RepoId, Actions)]

type RepoDesc = String

{- Actions that can be performed on a repo in the list. -}
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
	| UnwantedRepoActions
		{ setupRepoLink :: Route WebApp }

mkSyncingRepoActions :: RepoId -> Actions
mkSyncingRepoActions repoid = SyncingRepoActions
	{ setupRepoLink = EditRepositoryR repoid
	, syncToggleLink = DisableSyncR repoid
	}

mkNotSyncingRepoActions :: RepoId -> Actions
mkNotSyncingRepoActions repoid = NotSyncingRepoActions
	{ setupRepoLink = EditRepositoryR repoid
	, syncToggleLink = EnableSyncR repoid
	}

mkUnwantedRepoActions :: RepoId -> Actions
mkUnwantedRepoActions repoid = UnwantedRepoActions
	{ setupRepoLink = EditRepositoryR repoid
	}

needsEnabled :: Actions -> Bool
needsEnabled (DisabledRepoActions _) = True
needsEnabled _ = False

notSyncing :: Actions -> Bool
notSyncing (SyncingRepoActions _ _) = False
notSyncing _ = True

notWanted :: Actions -> Bool
notWanted (UnwantedRepoActions _) = True
notWanted _ = False

{- Called by client to get a list of repos, that refreshes
 - when new repos are added.
 -
 - Returns a div, which will be inserted into the calling page.
 -}
getRepoListR :: NotificationId -> RepoSelector -> Handler Html
getRepoListR nid reposelector = do
	waitNotifier getRepoListBroadcaster nid
	p <- widgetToPageContent $ repoListDisplay reposelector
	giveUrlRenderer $ [hamlet|^{pageBody p}|]

mainRepoSelector :: RepoSelector
mainRepoSelector = RepoSelector
	{ onlyCloud = False
	, onlyConfigured = False
	, includeHere = True
	, nudgeAddMore = False
	}

{- List of cloud repositories, configured and not. -}
cloudRepoList :: Widget
cloudRepoList = repoListDisplay RepoSelector
	{ onlyCloud = True
	, onlyConfigured = False
	, includeHere = False
	, nudgeAddMore = False
	}

repoListDisplay :: RepoSelector -> Widget
repoListDisplay reposelector = do
	autoUpdate ident (NotifierRepoListR reposelector) (10 :: Int) (10 :: Int)
	addScript $ StaticR jquery_ui_core_js
	addScript $ StaticR jquery_ui_widget_js
	addScript $ StaticR jquery_ui_mouse_js
	addScript $ StaticR jquery_ui_sortable_js

	repolist <- liftH $ repoList reposelector
	let addmore = nudgeAddMore reposelector
	let nootherrepos = length repolist < 2

	$(widgetFile "repolist")
  where
	ident = "repolist"

{- A list of known repositories, with actions that can be taken on them. -}
repoList :: RepoSelector -> Handler RepoList
repoList reposelector
	| onlyConfigured reposelector = list =<< configured
	| otherwise = list =<< (++) <$> configured <*> unconfigured
  where
	configured = do
		syncremotes <- syncRemotes <$> liftAssistant getDaemonStatus
		let syncing = S.fromList $ map mkRepoId syncremotes
		liftAnnex $ do
			unwanted <- S.fromList
				<$> filterM inUnwantedGroup (map Remote.uuid syncremotes)
			rs <- filter selectedrepo . concat . Remote.byCost
				<$> Remote.remoteList
			let l = flip map (map mkRepoId rs) $ \r -> case r of
				(RepoUUID u)
					| u `S.member` unwanted -> (r, mkUnwantedRepoActions r)
				_
					| r `S.member` syncing -> (r, mkSyncingRepoActions r)
					| otherwise -> (r, mkNotSyncingRepoActions r)
			if includeHere reposelector
				then do
					r <- RepoUUID <$> getUUID
					autocommit <- annexAutoCommit <$> Annex.getGitConfig
					let hereactions = if autocommit
						then mkSyncingRepoActions r
						else mkNotSyncingRepoActions r
					let here = (r, hereactions)
					return $ here : l
				else return l
	unconfigured = liftAnnex $ do
		m <- readRemoteLog
		g <- gitRepo
		map snd . catMaybes . filter selectedremote 
			. map (findinfo m g)
			<$> trustExclude DeadTrusted (M.keys m)
	selectedrepo r
		| Remote.readonly r = False
		| onlyCloud reposelector = Git.repoIsUrl (Remote.repo r)
			&& Remote.uuid r /= NoUUID 
			&& not (isXMPPRemote r)
		| otherwise = True
	selectedremote Nothing = False
	selectedremote (Just (iscloud, _))
		| onlyCloud reposelector = iscloud
		| otherwise = True
	findinfo m g u = case getconfig "type" of
		Just "rsync" -> val True EnableRsyncR
		Just "directory" -> val False EnableDirectoryR
#ifdef WITH_S3
		Just "S3" -> val True EnableS3R
#endif
		Just "glacier" -> val True EnableGlacierR
#ifdef WITH_WEBDAV
		Just "webdav" -> val True EnableWebDAVR
#endif
		Just "gcrypt" ->
			-- Skip gcrypt repos on removable drives;
			-- handled separately.
			case getconfig "gitrepo" of
				Just rr	| remoteLocationIsUrl (parseRemoteLocation rr g) ->
					val True EnableSshGCryptR
				_ -> Nothing
		_ -> Nothing
	  where
	  	getconfig k = M.lookup k =<< M.lookup u m
		val iscloud r = Just (iscloud, (RepoUUID u, DisabledRepoActions $ r u))
	list l = liftAnnex $
		forM (nubBy ((==) `on` fst) l) $ \(repoid, actions) ->
			(,,)
				<$> describeRepoId repoid
				<*> pure repoid
				<*> pure actions

getEnableSyncR :: RepoId -> Handler ()
getEnableSyncR = flipSync True

getDisableSyncR :: RepoId -> Handler ()
getDisableSyncR = flipSync False

flipSync :: Bool -> RepoId -> Handler ()
flipSync enable repoid = do
	mremote <- liftAnnex $ repoIdRemote repoid
	liftAssistant $ changeSyncable mremote enable
	redirectBack

getRepositoriesReorderR :: Handler ()
getRepositoriesReorderR = do
	{- Get uuid of the moved item, and the list it was moved within. -}
	moved <- fromjs <$> runInputGet (ireq textField "moved")
	list <- map fromjs <$> lookupGetParams "list[]"
	liftAnnex $ go list =<< Remote.remoteFromUUID moved
	liftAssistant updateSyncRemotes
  where
	go _ Nothing = noop
  	go list (Just remote) = do
		rs <- catMaybes <$> mapM Remote.remoteFromUUID list
		forM_ (reorderCosts remote rs) $ \(r, newcost) ->
			when (Remote.cost r /= newcost) $
				setRemoteCost r newcost
		void remoteListRefresh
  	fromjs = toUUID . T.unpack

reorderCosts :: Remote -> [Remote] -> [(Remote, Cost)]
reorderCosts remote rs = zip rs'' (insertCostAfter costs i)
  where
	{- Find the index of the remote in the list that the remote
	 - was moved to be after.
	 - If it was moved to the start of the list, -1 -}
	i = fromMaybe 0 (elemIndex remote rs) - 1
	rs' = filter (\r -> Remote.uuid r /= Remote.uuid remote) rs
	costs = map Remote.cost rs'
	rs'' = (\(x, y) -> x ++ [remote] ++ y) $ splitAt (i + 1) rs'

