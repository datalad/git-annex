{- git-annex assistant upgrading
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Upgrade where

import Assistant.Common
import qualified Annex
import Assistant.Threads.Watcher
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.NamedThread
import Utility.ThreadScheduler
import Utility.NotificationBroadcaster
import Utility.Url
import Utility.Env
import Git
import qualified Git.Construct
import qualified Git.Config
import Config.Files
import Types.Distribution

import Control.Concurrent
import System.Posix (getProcessID, signalProcess, sigTERM)
import System.Process (cwd)

{- Before the new assistant can be started, have to remove our 
 - gitAnnexUrlFile and our gitAnnexPidFile. Pausing the watcher is also
 - a good idea, to avoid fighting when two assistants are running in the
 - same repo.
 -}
prepUpgrade :: Assistant ()
prepUpgrade = do
	void $ addAlert upgradingAlert
	liftIO . maybe noop (`throwTo` PauseWatcher) =<< namedThreadId watchThread
	liftIO . nukeFile =<< liftAnnex (fromRepo gitAnnexUrlFile)
	liftIO . nukeFile =<< liftAnnex (fromRepo gitAnnexPidFile)
	void $ liftIO $ setEnv upgradedEnv "1" True

{- To finish an upgrade, send a global redirect to the new url
 - to any web browsers that are displaying the webapp.
 -
 - Wait for browser to update before terminating this process. -}
postUpgrade :: URLString -> Assistant ()
postUpgrade url = do
	modifyDaemonStatus_ $ \status -> status { globalRedirUrl = Just url }
	liftIO . sendNotification . globalRedirNotifier =<< getDaemonStatus
	void $ liftIO $ forkIO $ do
		threadDelaySeconds (Seconds 120)
		signalProcess sigTERM =<< getProcessID

{- Upgrade without interaction in the webapp. -}
unattendedUpgrade :: Assistant ()
unattendedUpgrade = do
	prepUpgrade
	url <- liftIO . newAssistantUrl
		=<< liftAnnex (repoLocation <$> Annex.gitRepo)
	postUpgrade url

autoUpgradeEnabled :: Assistant Bool
autoUpgradeEnabled = liftAnnex $ (==) AutoUpgrade . annexAutoUpgrade <$> Annex.getGitConfig

checkSuccessfulUpgrade :: IO Bool
checkSuccessfulUpgrade = isJust <$> getEnv upgradedEnv

upgradedEnv :: String
upgradedEnv = "GIT_ANNEX_UPGRADED"

{- Starts up the assistant in the repository, and waits for it to create
 - a gitAnnexUrlFile. Waits for the assistant to be up and listening for
 - connections by testing the url. -}
newAssistantUrl :: FilePath -> IO URLString
newAssistantUrl repo = do
	startAssistant repo
	geturl
  where
	geturl = do
		r <- Git.Config.read =<< Git.Construct.fromPath repo
		waiturl $ gitAnnexUrlFile r
	waiturl urlfile = do
		v <- tryIO $ readFile urlfile
		case v of
			Left _ -> delayed $ waiturl urlfile
			Right url -> ifM (listening url)
				( return url
				, delayed $ waiturl urlfile
				)
	listening url = catchBoolIO $ fst <$> exists url [] Nothing
	delayed a = do
		threadDelay 100000 -- 1/10th of a second
		a

{- Returns once the assistant has daemonized, but possibly before it's
 - listening for web connections. -}
startAssistant :: FilePath -> IO ()
startAssistant repo = do
	program <- readProgramFile
	(_, _, _, pid) <- 
		createProcess $
			(proc program ["assistant"]) { cwd = Just repo }
	void $ checkSuccessProcess pid
