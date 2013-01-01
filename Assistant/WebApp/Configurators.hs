{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes, CPP #-}

module Assistant.WebApp.Configurators where

import Assistant.WebApp.Common
import Assistant.DaemonStatus
import Assistant.WebApp.Notifications
import Assistant.WebApp.Utility
import Assistant.WebApp.Configurators.Local
import qualified Remote
import qualified Types.Remote as Remote
import Annex.UUID (getUUID)
import Logs.Remote
import Logs.Trust
import qualified Git
#ifdef WITH_XMPP
import Assistant.XMPP.Client
#endif

import qualified Data.Map as M

{- The main configuration screen. -}
getConfigurationR :: Handler RepHtml
getConfigurationR = ifM (inFirstRun)
	( getFirstRepositoryR
	, page "Configuration" (Just Configuration) $ do
#ifdef WITH_XMPP
		xmppconfigured <- lift $ runAnnex False $ isJust <$> getXMPPCreds
#else
		let xmppconfigured = False
#endif
		$(widgetFile "configurators/main")
	)

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

makeCloudRepositories :: Widget
makeCloudRepositories = $(widgetFile "configurators/repositories/cloud")

{- Lists known repositories, followed by options to add more. -}
getRepositoriesR :: Handler RepHtml
getRepositoriesR = page "Repositories" (Just Configuration) $ do
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

	repolist <- lift $ repoList reposelector

	$(widgetFile "configurators/repositories/list")

  where
	ident = "repolist"

type RepoList = [(String, String, Actions)]

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
		runAnnex [] $ do
			u <- getUUID
			let l = map Remote.uuid rs
			let l' = if includeHere reposelector then u : l else l
			return $ zip l' $ map mkSyncingRepoActions l'
	rest = runAnnex [] $ do
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
	list l = runAnnex [] $ do
		let l' = nubBy (\x y -> fst x == fst y) l
		zip3
			<$> pure counter
			<*> Remote.prettyListUUIDs (map fst l')
			<*> pure (map snd l')
	counter = map show ([1..] :: [Int])

getEnableSyncR :: UUID -> Handler ()
getEnableSyncR = flipSync True

getDisableSyncR :: UUID -> Handler ()
getDisableSyncR = flipSync False

flipSync :: Bool -> UUID -> Handler ()
flipSync enable uuid = do
	mremote <- runAnnex undefined $ Remote.remoteFromUUID uuid
	changeSyncable mremote enable
	redirect RepositoriesR
