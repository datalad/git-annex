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
 - Thread 2: watcher
 - 	Notices new files, and calls handlers for events, queuing changes.
 - Thread 3: inotify internal
 - 	Used by haskell inotify library to ensure inotify event buffer is
 - 	kept drained.
 - Thread 4: inotify startup scanner
 - 	Scans the tree and registers inotify watches for each directory.
 -	A MVar lock is used to prevent other inotify handlers from running
 -	until this is complete.
 - Thread 5: committer
 - 	Waits for changes to occur, and runs the git queue to update its
 - 	index, then commits.
 - Thread 6: pusher
 - 	Waits for commits to be made, and pushes updated branches to remotes,
 - 	in parallel. (Forks a process for each git push.)
 - Thread 7: push retryer
 - 	Runs every 30 minutes when there are failed pushes, and retries
 - 	them.
 - Thread 8: merger
 - 	Waits for pushes to be received from remotes, and merges the
 - 	updated branches into the current branch. This uses inotify
 - 	on .git/refs/heads, so there are additional inotify threads
 - 	associated with it, too.
 - Thread 9: status logger
 - 	Wakes up periodically and records the daemon's status to disk.
 - Thread 10: sanity checker
 - 	Wakes up periodically (rarely) and does sanity checks.
 -
 - ThreadState: (MVar)
 - 	The Annex state is stored here, which allows resuscitating the
 - 	Annex monad in IO actions run by the watcher and committer
 - 	threads. Thus, a single state is shared amoung the threads, and
 - 	only one at a time can access it.
 - DaemonStatusHandle: (MVar)
 - 	The daemon's current status. This MVar should only be manipulated
 - 	from inside the Annex monad, which ensures it's accessed only
 - 	after the ThreadState MVar.
 - ChangeChan: (STM TChan)
 - 	Changes are indicated by writing to this channel. The committer
 - 	reads from it.
 - CommitChan: (STM TChan)
 - 	Commits are indicated by writing to this channel. The pusher reads
 - 	from it.
 - FailedPushMap (STM TMVar)
 - 	Failed pushes are indicated by writing to this TMVar. The push
 - 	retrier blocks until they're available.
 -}

module Assistant where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.Commits
import Assistant.Pushes
import Assistant.Threads.Watcher
import Assistant.Threads.Committer
import Assistant.Threads.Pusher
import Assistant.Threads.Merger
import Assistant.Threads.SanityChecker
import qualified Utility.Daemon
import Utility.LogFile
import Utility.ThreadScheduler

import Control.Concurrent

startDaemon :: Bool -> Bool -> Annex ()
startDaemon assistant foreground
	| foreground = do
		showStart (if assistant then "assistant" else "watch") "."
		go id
	| otherwise = do
		logfd <- liftIO . openLog =<< fromRepo gitAnnexLogFile
		pidfile <- fromRepo gitAnnexPidFile
		go $ Utility.Daemon.daemonize logfd (Just pidfile) False
	where
		go a = withThreadState $ \st -> do
			checkCanWatch
			dstatus <- startDaemonStatus
			liftIO $ a $ do
				changechan <- newChangeChan
				commitchan <- newCommitChan
				pushmap <- newFailedPushMap
				_ <- forkIO $ commitThread st changechan commitchan
				_ <- forkIO $ pushThread st commitchan pushmap
				_ <- forkIO $ pushRetryThread st pushmap
				_ <- forkIO $ mergeThread st
				_ <- forkIO $ daemonStatusThread st dstatus
				_ <- forkIO $ sanityCheckerThread st dstatus changechan
				_ <- forkIO $ watchThread st dstatus changechan
				waitForTermination

stopDaemon :: Annex ()
stopDaemon = liftIO . Utility.Daemon.stopDaemon =<< fromRepo gitAnnexPidFile
