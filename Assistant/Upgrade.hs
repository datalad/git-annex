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
import Utility.Env
import Git
import Config.Files

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

{- Wait for browser to update before terminating this process. -}
postUpgrade :: IO ()
postUpgrade = void $ forkIO $ do
	threadDelaySeconds (Seconds 120)
	signalProcess sigTERM =<< getProcessID

{- Upgrade without interaction in the webapp.
 - 
 - XXX If the webapp is open, this will make it stop working
 - or close, with no more indication why than an alert.
 -}
unattendedUpgrade :: Assistant ()
unattendedUpgrade = do
	prepUpgrade
	liftIO . startAssistant =<< liftAnnex (repoLocation <$> Annex.gitRepo)
	liftIO postUpgrade

{- Returns once the assistant has daemonized, but possibly before it's
 - listening for web connections. -}
startAssistant :: FilePath -> IO ()
startAssistant repo = do
	program <- readProgramFile
	(_, _, _, pid) <- 
		createProcess $
			(proc program ["assistant"]) { cwd = Just repo }
	void $ checkSuccessProcess pid

checkSuccessfulUpgrade :: IO Bool
checkSuccessfulUpgrade = isJust <$> getEnv upgradedEnv

upgradedEnv :: String
upgradedEnv = "GIT_ANNEX_UPGRADED"
