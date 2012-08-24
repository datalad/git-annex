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
	startupScan
	runEvery (Seconds 2) $ do
		(r, info) <- getScanRemote scanremotes
		scanned <- runThreadState st $ inRepo $
			checkTransferScanned $ Remote.uuid r
		if not scanned || fullScan info
			then expensiveScan st dstatus transferqueue r
			else failedTransferScan st dstatus transferqueue r
	where
		{- All remotes are scanned in full on startup, for multiple
		 - reasons, including:
		 -
		 - * This may be the first run, and there may be remotes
		 -   already in place, that need to be synced.
		 - * We may have run before, and scanned a remote, but
		 -   only been in a subdirectory of the git remote, and so
		 -   not synced it all.
		 - * We may have run before, and had transfers queued,
		 -   and then the system (or us) crashed, and that info was
		 -   lost.
		 -
		 - But not all remotes may be available now. So all
		 - prior indications that remotes have been scanned
		 - are first removed.
		 -}
		startupScan = do
			void $ tryIO $ 
				removeDirectoryRecursive
					=<< runThreadState st (fromRepo transferScannedDir)
			addScanRemotes scanremotes True
				=<< knownRemotes <$> getDaemonStatus dstatus

{- This is a cheap scan for failed transfers involving a remote. -}
failedTransferScan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Remote -> IO ()
failedTransferScan st dstatus transferqueue r = do
	ts <- runThreadState st $ 
		getFailedTransfers $ Remote.uuid r
	go ts
	where
		go [] = noop
		go ((t, info):ts)
			| transferDirection t == Download = do
				{- Check if the remote still has the key.
				 - If not, relies on the expensiveScan to
				 - get it queued from some other remote. -}
				ifM (runThreadState st $ remoteHas r $ transferKey t)
					( requeue t info
					, dequeue t
					)
				go ts
			| otherwise = do
				{- The Transferrer checks when uploading
				 - that the remote doesn't already have the
				 - key, so it's not redundantly checked
				 - here. -}
				requeue t info
				go ts

		requeue t info = do
			queueTransferWhenSmall
				transferqueue dstatus (associatedFile info) t r
			dequeue t
		dequeue t = void $ runThreadState st $ inRepo $
			liftIO . tryIO . removeFile . failedTransferFile t

{- This is a expensive scan through the full git work tree.
 - 
 - The scan is blocked when the transfer queue gets too large. -}
expensiveScan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Remote -> IO ()
expensiveScan st dstatus transferqueue r = do
	liftIO $ debug thisThread ["starting scan of", show r]
	void $ alertWhile dstatus (scanAlert r) $ do
		g <- runThreadState st $ fromRepo id
		files <- LsFiles.inRepo [] g
		go files
		runThreadState st $ inRepo $
			transferScanned $ Remote.uuid r
		return True
	liftIO $ debug thisThread ["finished scan of", show r]
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
					( helper key Upload False =<< remoteHas r key
					, helper key Download True =<< remoteHas r key
					)
				helper key direction x y
					| x == y = return $ Just $
						Transfer direction (Remote.uuid r) key
					| otherwise = return Nothing
				
		enqueue f t = queueTransferWhenSmall transferqueue dstatus (Just f) t r

remoteHas :: Remote -> Key -> Annex Bool
remoteHas r key = elem
	<$> pure (Remote.uuid r)
	<*> loggedLocations key
