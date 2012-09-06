{- git-annex assistant daemon status thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.DaemonStatus where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.ThreadedMonad
import Utility.ThreadScheduler
import Utility.NotificationBroadcaster

thisThread :: ThreadName
thisThread = "DaemonStatus"

{- This writes the daemon status to disk, when it changes, but no more
 - frequently than once every ten minutes.
 -}
daemonStatusThread :: ThreadState -> DaemonStatusHandle -> NamedThread
daemonStatusThread st dstatus = thread $ do
	notifier <- newNotificationHandle
		=<< changeNotifier <$> getDaemonStatus dstatus
	checkpoint
	runEvery (Seconds tenMinutes) $ do
		waitNotification notifier
		checkpoint
	where
		thread = NamedThread thisThread
		checkpoint = do
			status <- getDaemonStatus dstatus
			file <- runThreadState st $ fromRepo gitAnnexDaemonStatusFile
			writeDaemonStatusFile file status

