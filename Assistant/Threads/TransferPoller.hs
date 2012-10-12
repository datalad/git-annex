{- git-annex assistant transfer polling thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferPoller where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Logs.Transfer
import Utility.NotificationBroadcaster
import qualified Assistant.Threads.TransferWatcher as TransferWatcher

import Control.Concurrent
import qualified Data.Map as M

thisThread :: ThreadName
thisThread = "TransferPoller"

{- This thread polls the status of ongoing transfers, determining how much
 - of each transfer is complete. -}
transferPollerThread :: ThreadState -> DaemonStatusHandle -> NamedThread
transferPollerThread st dstatus = thread $ do
	g <- runThreadState st gitRepo
	tn <- newNotificationHandle =<<
		transferNotifier <$> getDaemonStatus dstatus
	forever $ do
		threadDelay 500000 -- 0.5 seconds
		ts <- currentTransfers <$> getDaemonStatus dstatus
		if M.null ts
			then waitNotification tn -- block until transfers running
			else mapM_ (poll g) $ M.toList ts
	where
		thread = NamedThread thisThread
		poll g (t, info)
			{- Downloads are polled by checking the size of the
			 - temp file being used for the transfer. -}
			| transferDirection t == Download = do
				let f = gitAnnexTmpLocation (transferKey t) g
				sz <- catchMaybeIO $
					fromIntegral . fileSize
						<$> getFileStatus f
				newsize t info sz
			{- Uploads don't need to be polled for when the
			 - TransferWatcher thread can track file
			 - modifications. -}
			| TransferWatcher.watchesTransferSize = noop
			{- Otherwise, this code polls the upload progress
			 - by reading the transfer info file. -}
			| otherwise = do
				let f = transferFile t g
				mi <- catchDefaultIO Nothing $
					readTransferInfoFile Nothing f
				maybe noop (newsize t info . bytesComplete) mi
		newsize t info sz
			| bytesComplete info /= sz && isJust sz = 
				alterTransferInfo dstatus t $
					\i -> i { bytesComplete = sz }
			| otherwise = noop
