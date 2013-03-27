{- git-annex assistant daemon status thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.DaemonStatus where

import Assistant.Common
import Assistant.DaemonStatus
import Utility.ThreadScheduler
import Utility.NotificationBroadcaster

{- This writes the daemon status to disk, when it changes, but no more
 - frequently than once every ten minutes.
 -}
daemonStatusThread :: NamedThread
daemonStatusThread = namedThread "DaemonStatus" $ do
	notifier <- liftIO . newNotificationHandle False
		=<< changeNotifier <$> getDaemonStatus
	checkpoint
	runEvery (Seconds tenMinutes) <~> do
		liftIO $ waitNotification notifier
		checkpoint
  where
	checkpoint = do
		file <- liftAnnex $ fromRepo gitAnnexDaemonStatusFile
		liftIO . writeDaemonStatusFile file =<< getDaemonStatus
