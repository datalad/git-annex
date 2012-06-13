{- git-annex assistant daemon
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -
 - Overview of threads and MVars, etc:
 -
 - Thread 1: parent
 - 	The initial thread run, double forks to background, starts other
 - 	threads, and then stops, waiting for them to terminate,
 - 	or for a ctrl-c.
 - Thread 2: inotify
 - 	Notices new files, and calls handlers for events, queuing changes.
 - Thread 3: inotify internal
 - 	Used by haskell inotify library to ensure inotify event buffer is
 - 	kept drained.
 - Thread 4: inotify initial scan
 -	A MVar lock is used to prevent other inotify handlers from running
 -	until this is complete.
 - Thread 5: committer
 - 	Waits for changes to occur, and runs the git queue to update its
 - 	index, then commits.
 - Thread 6: status logger
 - 	Wakes up periodically and records the daemon's status to disk.
 -
 - ThreadState: (MVar)
 - 	The Annex state is stored here, which allows resuscitating the
 - 	Annex monad in IO actions run by the inotify and committer
 - 	threads. Thus, a single state is shared amoung the threads, and
 - 	only one at a time can access it.
 - DaemonStatusHandle: (MVar)
 - 	The daemon's current status. This MVar should only be manipulated
 - 	from inside the Annex monad, which ensures it's accessed only
 - 	after the ThreadState MVar.
 - ChangeChan: (STM TChan)
 - 	Changes are indicated by writing to this channel. The committer
 - 	reads from it.
 -}

module Assistant where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Watcher
import Assistant.Committer
import qualified Utility.Daemon
import Utility.LogFile

import Control.Concurrent

startDaemon :: Bool -> Annex ()
startDaemon foreground
	| foreground = do
		showStart "watch" "."
		go id
	| otherwise = do
		logfd <- liftIO . openLog =<< fromRepo gitAnnexLogFile
		pidfile <- fromRepo gitAnnexPidFile
		go $ Utility.Daemon.daemonize logfd (Just pidfile) False
	where
		go a = withThreadState $ \st -> do
			dstatus <- startDaemonStatus
			liftIO $ a $ do
				changechan <- newChangeChan
				-- The commit thread is started early,
				-- so that the user can immediately
				-- begin adding files and having them
				-- committed, even while the startup scan
				-- is taking place.
				_ <- forkIO $ commitThread st changechan
				_ <- forkIO $ daemonStatusThread st dstatus
				watchThread st dstatus changechan

stopDaemon :: Annex ()
stopDaemon = liftIO . Utility.Daemon.stopDaemon =<< fromRepo gitAnnexPidFile
