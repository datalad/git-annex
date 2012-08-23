{- git-annex assistant thread to scan remotes to find needed transfers
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferScanner where

import Assistant.Common
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Alert
import Logs.Transfer
import Logs.Location
import qualified Remote
import Utility.ThreadScheduler
import qualified Git.LsFiles as LsFiles
import Command
import Annex.Content

thisThread :: ThreadName
thisThread = "TransferScanner"

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -}
transferScannerThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> TransferQueue -> IO ()
transferScannerThread st dstatus scanremotes transferqueue = do
	runEvery (Seconds 2) $ do
		(r, info) <- getScanRemote scanremotes
		scanned <- runThreadState st $ inRepo $
			checkTransferScanned $ Remote.uuid r
		if not scanned || fullScan info
			then do
				liftIO $ debug thisThread ["starting scan of", show r]
				void $ alertWhile dstatus (scanAlert r) $
					expensiveScan st dstatus transferqueue r
				liftIO $ debug thisThread ["finished scan of", show r]
				runThreadState st $ inRepo $
					transferScanned $ Remote.uuid r
			else failedTransferScan st dstatus transferqueue r

{- This is a cheap scan for failed transfers involving a remote. -}
failedTransferScan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Remote -> IO ()
failedTransferScan st dstatus transferqueue r = do
	ts <- runThreadState st $ 
		getFailedTransfers $ Remote.uuid r
	go ts
	where
		go [] = noop
		go ((t, info):ts) = do
			queueTransferWhenSmall
				transferqueue dstatus (associatedFile info) t r
			void $ runThreadState st $ inRepo $
				liftIO . tryIO . removeFile . failedTransferFile t
			go ts

{- This is a expensive scan through the full git work tree.
 - 
 - The scan is blocked when the transfer queue gets too large. -}
expensiveScan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Remote -> IO Bool
expensiveScan st dstatus transferqueue r = do
	g <- runThreadState st $ fromRepo id
	files <- LsFiles.inRepo [] g
	go files
	return True
	where
		go [] = noop
		go (f:fs) = do
			v <- runThreadState st $ whenAnnexed check f
			case v of
				Nothing -> noop
				Just t -> do
					debug thisThread ["queuing", show t]
					enqueue f t
			go fs
			where
				check _ (key, _) = ifM (inAnnex key)
					( helper key Upload False =<< remotehas key
					, helper key Download True =<< remotehas key
					)
				helper key direction x y
					| x == y = return $
						Just $ Transfer direction u key
					| otherwise = return Nothing
				
		u = Remote.uuid r
		enqueue f t = queueTransferWhenSmall transferqueue dstatus (Just f) t r

		{- Look directly in remote for the key when it's cheap;
		 - otherwise rely on the location log. -}
		remotehas key
			| Remote.hasKeyCheap r = (==)
				<$> pure (Right True)
				<*> Remote.hasKey r key
			| otherwise = elem
				<$> pure u
				<*> loggedLocations key
