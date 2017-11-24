{- git-annex assistant transfer polling thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferPoller where

import Assistant.Common
import Assistant.DaemonStatus
import Types.Transfer
import Logs.Transfer
import Utility.NotificationBroadcaster
import qualified Assistant.Threads.TransferWatcher as TransferWatcher

import Control.Concurrent
import qualified Data.Map as M

{- This thread polls the status of ongoing transfers, determining how much
 - of each transfer is complete. -}
transferPollerThread :: NamedThread
transferPollerThread = namedThread "TransferPoller" $ do
	g <- liftAnnex gitRepo
	tn <- liftIO . newNotificationHandle True =<<
		transferNotifier <$> getDaemonStatus
	forever $ do
		liftIO $ threadDelay 500000 -- 0.5 seconds
		ts <- currentTransfers <$> getDaemonStatus
		if M.null ts
			-- block until transfers running
			then liftIO $ waitNotification tn
			else mapM_ (poll g) $ M.toList ts
  where
	poll g (t, info)
		{- Downloads are polled by checking the size of the
		 - temp file being used for the transfer. -}
		| transferDirection t == Download = do
			let f = gitAnnexTmpObjectLocation (transferKey t) g
			sz <- liftIO $ catchMaybeIO $ getFileSize f
			newsize t info sz
		{- Uploads don't need to be polled for when the TransferWatcher
		 - thread can track file modifications. -}
		| TransferWatcher.watchesTransferSize = noop
		{- Otherwise, this code polls the upload progress
		 - by reading the transfer info file. -}
		| otherwise = do
			let f = transferFile t g
			mi <- liftIO $ catchDefaultIO Nothing $
				readTransferInfoFile Nothing f
			maybe noop (newsize t info . bytesComplete) mi

	newsize t info sz
		| bytesComplete info /= sz && isJust sz =
			alterTransferInfo t $ \i -> i { bytesComplete = sz }
		| otherwise = noop
