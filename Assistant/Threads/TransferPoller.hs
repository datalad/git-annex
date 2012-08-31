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

import Control.Concurrent
import qualified Data.Map as M

thisThread :: ThreadName
thisThread = "TransferPoller"

{- This thread polls the status of ongoing transfers, determining how much
 - of each transfer is complete. -}
transferPollerThread :: ThreadState -> DaemonStatusHandle -> IO ()
transferPollerThread st dstatus = do
	g <- runThreadState st $ fromRepo id
	tn <- newNotificationHandle =<<
		transferNotifier <$> getDaemonStatus dstatus
	forever $ do
		threadDelay 500000 -- 0.5 seconds
		ts <- currentTransfers <$> getDaemonStatus dstatus
		if M.null ts
			then waitNotification tn -- block until transfers running
			else mapM_ (poll g) $ M.toList ts
	where
		poll g (t, info)
			{- Downloads are polled by checking the size of the
			 - temp file being used for the transfer. -}
			| transferDirection t == Download = do
				let f = gitAnnexTmpLocation (transferKey t) g
				sz <- catchMaybeIO $
					fromIntegral . fileSize
						<$> getFileStatus f
				when (bytesComplete info /= sz && isJust sz) $
					alterTransferInfo dstatus t $
						\i -> i { bytesComplete = sz }
			{- can't poll uploads -}
			| otherwise = noop
