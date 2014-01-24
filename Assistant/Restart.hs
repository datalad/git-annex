{- git-annex assistant restarting
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Restart where

import Assistant.Common
import Assistant.Threads.Watcher
import Assistant.DaemonStatus
import Assistant.NamedThread
import Utility.ThreadScheduler
import Utility.NotificationBroadcaster
import Utility.Url
import qualified Git.Construct
import qualified Git.Config
import Config.Files
import qualified Annex
import qualified Git

import Control.Concurrent
import System.Process (cwd)
#ifndef mingw32_HOST_OS
import System.Posix (getProcessID, signalProcess, sigTERM)
#else
import System.Win32.Process.Current (getCurrentProcessId)
import System.Win32.Console (generateConsoleCtrlEvent, cTRL_C_EVENT)
#endif

{- Before the assistant can be restarted, have to remove our 
 - gitAnnexUrlFile and our gitAnnexPidFile. Pausing the watcher is also
 - a good idea, to avoid fighting when two assistants are running in the
 - same repo.
 -}
prepRestart :: Assistant ()
prepRestart = do
	liftIO . maybe noop (`throwTo` PauseWatcher) =<< namedThreadId watchThread
	liftIO . nukeFile =<< liftAnnex (fromRepo gitAnnexUrlFile)
	liftIO . nukeFile =<< liftAnnex (fromRepo gitAnnexPidFile)

{- To finish a restart, send a global redirect to the new url
 - to any web browsers that are displaying the webapp.
 -
 - Wait for browser to update before terminating this process. -}
postRestart :: URLString -> Assistant ()
postRestart url = do
	modifyDaemonStatus_ $ \status -> status { globalRedirUrl = Just url }
	liftIO . sendNotification . globalRedirNotifier =<< getDaemonStatus
	void $ liftIO $ forkIO $ do
		threadDelaySeconds (Seconds 120)
#ifndef mingw32_HOST_OS
		signalProcess sigTERM =<< getProcessID
#else
		generateConsoleCtrlEvent cTRL_C_EVENT =<< getCurrentProcessId
#endif

runRestart :: Assistant URLString
runRestart = liftIO . newAssistantUrl
	=<< liftAnnex (Git.repoLocation <$> Annex.gitRepo)

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
