{- git-annex assistant restarting
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
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
import Utility.PID
import qualified Git.Construct
import qualified Git.Config
import qualified Annex
import qualified Git
import Annex.Path

import Control.Concurrent
#ifndef mingw32_HOST_OS
import System.Posix (signalProcess, sigTERM)
#else
import Utility.WinProcess
#endif
import Network.URI

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
		terminateSelf

terminateSelf :: IO ()
terminateSelf =
#ifndef mingw32_HOST_OS
		signalProcess sigTERM =<< getPID
#else
		terminatePID =<< getPID
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
			Right url -> ifM (assistantListening url)
				( return url
				, delayed $ waiturl urlfile
				)
	delayed a = do
		threadDelay 100000 -- 1/10th of a second
		a

{- Checks if the assistant is listening on an url.
 -
 - Always checks http, because https with self-signed cert is problematic.
 - warp-tls listens to http, in order to show an error page, so this works.
 -}
assistantListening :: URLString -> IO Bool
assistantListening url = catchBoolIO $ exists url' def
  where
	url' = case parseURI url of
		Nothing -> url
		Just uri -> show $ uri
			{ uriScheme = "http:"
			}

{- Does not wait for assistant to be listening for web connections. 
 -
 - On windows, the assistant does not daemonize, which is why the forkIO is
 - done.
 -}
startAssistant :: FilePath -> IO ()
startAssistant repo = void $ forkIO $ do
	program <- programPath
	(_, _, _, pid) <- 
		createProcess $
			(proc program ["assistant"]) { cwd = Just repo }
	void $ checkSuccessProcess pid
