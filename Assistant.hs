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
 - Thread 2: Watcher
 - 	Notices new files, and calls handlers for events, queuing changes.
 - Thread 3: inotify internal
 - 	Used by haskell inotify library to ensure inotify event buffer is
 - 	kept drained.
 - Thread 4: inotify startup scanner
 - 	Scans the tree and registers inotify watches for each directory.
 -	A MVar lock is used to prevent other inotify handlers from running
 -	until this is complete.
 - Thread 5: Committer
 - 	Waits for changes to occur, and runs the git queue to update its
 - 	index, then commits. Also queues Transfer events to send added
 - 	files to other remotes.
 - Thread 6: Pusher
 - 	Waits for commits to be made, and pushes updated branches to remotes,
 - 	in parallel. (Forks a process for each git push.)
 - Thread 7: PushRetryer
 - 	Runs every 30 minutes when there are failed pushes, and retries
 - 	them.
 - Thread 8: Merger
 - 	Waits for pushes to be received from remotes, and merges the
 - 	updated branches into the current branch.
 - 	(This uses inotify on .git/refs/heads, so there are additional
 - 	inotify threads associated with it, too.)
 - Thread 9: TransferWatcher
 - 	Watches for transfer information files being created and removed,
 - 	and maintains the DaemonStatus currentTransfers map.
 - 	(This uses inotify on .git/annex/transfer/, so there are
 - 	additional inotify threads associated with it, too.)
 - Thread 10: Transferrer
 - 	Waits for Transfers to be queued and does them.
 - Thread 11: StatusLogger
 - 	Wakes up periodically and records the daemon's status to disk.
 - Thread 12: SanityChecker
 - 	Wakes up periodically (rarely) and does sanity checks.
 - Thread 13: MountWatcher
 - 	Either uses dbus to watch for drive mount events, or, when
 - 	there's no dbus, polls to find newly mounted filesystems.
 - 	Once a filesystem that contains a remote is mounted, updates
 - 	state about that remote, pulls from it, and queues a push to it,
 - 	as well as an update, and queues it onto the
 - 	ConnectedRemoteChan
 - Thread 14: TransferScanner
 - 	Does potentially expensive checks to find data that needs to be
 - 	transferred from or to remotes, and queues Transfers.
 - 	Uses the ScanRemotes map.
 - Thread 15: WebApp
 - 	Spawns more threads as necessary to handle clients.
 - 	Displays the DaemonStatus.
 -
 - ThreadState: (MVar)
 - 	The Annex state is stored here, which allows resuscitating the
 - 	Annex monad in IO actions run by the watcher and committer
 - 	threads. Thus, a single state is shared amoung the threads, and
 - 	only one at a time can access it.
 - DaemonStatusHandle: (STM TMVar)
 - 	The daemon's current status.
 - ChangeChan: (STM TChan)
 - 	Changes are indicated by writing to this channel. The committer
 - 	reads from it.
 - CommitChan: (STM TChan)
 - 	Commits are indicated by writing to this channel. The pusher reads
 - 	from it.
 - FailedPushMap (STM TMVar)
 - 	Failed pushes are indicated by writing to this TMVar. The push
 - 	retrier blocks until they're available.
 - TransferQueue (STM TChan)
 - 	Transfers to make are indicated by writing to this channel.
 - TransferSlots (QSemN)
 - 	Count of the number of currently available transfer slots.
 - 	Updated by the transfer watcher, this allows other threads
 - 	to block until a slot is available.
 - 	This MVar should only be manipulated from inside the Annex monad,
 - 	which ensures it's accessed only after the ThreadState MVar.
 - ScanRemotes (STM TMVar)
 - 	Remotes that have been disconnected, and should be scanned
 - 	are indicated by writing to this TMVar.
 -}

{-# LANGUAGE CPP #-}

module Assistant where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.Commits
import Assistant.Pushes
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Threads.Watcher
import Assistant.Threads.Committer
import Assistant.Threads.Pusher
import Assistant.Threads.Merger
import Assistant.Threads.TransferWatcher
import Assistant.Threads.Transferrer
import Assistant.Threads.SanityChecker
import Assistant.Threads.MountWatcher
import Assistant.Threads.TransferScanner
#ifdef WITH_WEBAPP
import Assistant.Threads.WebApp
#else
#warning Building without the webapp. You probably need to install Yesod..
#endif
import qualified Utility.Daemon
import Utility.LogFile
import Utility.ThreadScheduler

import Control.Concurrent

stopDaemon :: Annex ()
stopDaemon = liftIO . Utility.Daemon.stopDaemon =<< fromRepo gitAnnexPidFile

startDaemon :: Bool -> Bool -> Maybe (Url -> FilePath -> IO ()) -> Annex ()
startDaemon assistant foreground webappwaiter
	| foreground = do
		showStart (if assistant then "assistant" else "watch") "."
		liftIO . Utility.Daemon.lockPidFile =<< fromRepo gitAnnexPidFile
		go id
	| otherwise = do
		logfd <- liftIO . openLog =<< fromRepo gitAnnexLogFile
		pidfile <- fromRepo gitAnnexPidFile
		go $ Utility.Daemon.daemonize logfd (Just pidfile) False
	where
		go d = startAssistant assistant d webappwaiter

startAssistant :: Bool -> (IO () -> IO ()) -> Maybe (Url -> FilePath -> IO ()) -> Annex ()
startAssistant assistant daemonize webappwaiter = do
	withThreadState $ \st -> do
		checkCanWatch
		dstatus <- startDaemonStatus
		liftIO $ daemonize $ run dstatus st
	where
		run dstatus st = do
			changechan <- newChangeChan
			commitchan <- newCommitChan
			pushmap <- newFailedPushMap
			transferqueue <- newTransferQueue
			transferslots <- newTransferSlots
			scanremotes <- newScanRemoteMap
			mapM_ startthread
				[ watch $ commitThread st changechan commitchan transferqueue dstatus
#ifdef WITH_WEBAPP
				, assist $ webAppThread (Just st) dstatus transferqueue Nothing webappwaiter
#endif
				, assist $ pushThread st dstatus commitchan pushmap
				, assist $ pushRetryThread st dstatus pushmap
				, assist $ mergeThread st
				, assist $ transferWatcherThread st dstatus
				, assist $ transfererThread st dstatus transferqueue transferslots
				, assist $ daemonStatusThread st dstatus
				, assist $ sanityCheckerThread st dstatus transferqueue changechan
				, assist $ mountWatcherThread st dstatus scanremotes
				, assist $ transferScannerThread st dstatus scanremotes transferqueue
				, watch $ watchThread st dstatus transferqueue changechan
				]
			waitForTermination
		watch a = (True, a)
		assist a = (False, a)
		startthread (watcher, a)
			| watcher || assistant = void $ forkIO a
			| otherwise = noop
