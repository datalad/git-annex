{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes, CPP #-}

module Assistant.WebApp.Configurators where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Assistant.WebApp.Utility
import Assistant.WebApp.Configurators.Local
import Utility.Yesod
import qualified Remote
import qualified Types.Remote as Remote
import Annex.UUID (getUUID)
import Logs.Remote
import Logs.Trust
import Config
#ifdef WITH_XMPP
import Assistant.XMPP.Client
#endif

import Yesod
import Data.Text (Text)
import qualified Data.Map as M

{- The main configuration screen. -}
getConfigR :: Handler RepHtml
getConfigR = ifM (inFirstRun)
	( getFirstRepositoryR
	, bootstrap (Just Config) $ do
#ifdef WITH_XMPP
		xmppconfigured <- lift $ runAnnex False $ isJust <$> getXMPPCreds
#else
		let xmppconfigured = False
#endif
		sideBarDisplay
		setTitle "Configuration"
		$(widgetFile "configurators/main")
	)

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	repolist <- lift $ repoList True False
	let n = length repolist
	let numrepos = show n
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }

{- Lists known repositories, followed by options to add more. -}
getRepositoriesR :: Handler RepHtml
getRepositoriesR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Repositories"
	repolist <- lift $ repoList False True
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

{- A numbered list of known repositories,
 - with actions that can be taken on them. -}
repoList :: Bool -> Bool -> Handler [(String, String, Actions)]
repoList onlyconfigured includehere
	| onlyconfigured = list =<< configured
	| otherwise = list =<< (++) <$> configured <*> rest
  where
	configured = do
		rs <- filter (not . Remote.readonly) . syncRemotes
			<$> liftAssistant getDaemonStatus
		runAnnex [] $ do
			u <- getUUID
			let l = map Remote.uuid rs
			let l' = if includehere then u : l else l
			return $ zip l' $ map mkSyncingRepoActions l'
	rest = runAnnex [] $ do
		m <- readRemoteLog
		unconfigured <- catMaybes . map (findtype m) . snd
			<$> (trustPartition DeadTrusted $ M.keys m)
		unsyncable <- map Remote.uuid <$>
			(filterM (\r -> not <$> repoSyncable (Remote.repo r))
				=<< Remote.enabledRemoteList)
		return $ zip unsyncable (map mkNotSyncingRepoActions unsyncable) ++ unconfigured
	findtype m u = case M.lookup u m of
		Nothing -> Nothing
		Just c -> case M.lookup "type" c of
			Just "rsync" -> u `enableswith` EnableRsyncR
			Just "directory" -> u `enableswith` EnableDirectoryR
#ifdef WITH_S3
			Just "S3" -> u `enableswith` EnableS3R
#endif
			_ -> Nothing
	u `enableswith` r = Just (u, DisabledRepoActions $ r u)
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
	mremote <- runAnnex undefined $ snd <$> Remote.repoFromUUID uuid
	changeSyncable mremote enable
	redirect RepositoriesR
