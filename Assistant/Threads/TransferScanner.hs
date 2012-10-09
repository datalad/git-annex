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
import Logs.Web (webUUID)
import qualified Remote
import qualified Types.Remote as Remote
import Utility.ThreadScheduler
import qualified Git.LsFiles as LsFiles
import Command
import Annex.Content
import Annex.Wanted

import qualified Data.Set as S

thisThread :: ThreadName
thisThread = "TransferScanner"

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -}
transferScannerThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> TransferQueue -> NamedThread
transferScannerThread st dstatus scanremotes transferqueue = thread $ do
	startupScan
	go S.empty
	where
		thread = NamedThread thisThread
		go scanned = do
			threadDelaySeconds (Seconds 2)
			(rs, infos) <- unzip <$> getScanRemote scanremotes
			if any fullScan infos || any (`S.notMember` scanned) rs
				then do
					expensiveScan st dstatus transferqueue rs
					go $ scanned `S.union` S.fromList rs
				else do
					mapM_ (failedTransferScan st dstatus transferqueue) rs
					go scanned
		{- All available remotes are scanned in full on startup,
		 - for multiple reasons, including:
		 -
		 - * This may be the first run, and there may be remotes
		 -   already in place, that need to be synced.
		 - * We may have run before, and scanned a remote, but
		 -   only been in a subdirectory of the git remote, and so
		 -   not synced it all.
		 - * We may have run before, and had transfers queued,
		 -   and then the system (or us) crashed, and that info was
		 -   lost.
		 -}
		startupScan = addScanRemotes scanremotes True
			=<< knownRemotes <$> getDaemonStatus dstatus

{- This is a cheap scan for failed transfers involving a remote. -}
failedTransferScan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Remote -> IO ()
failedTransferScan st dstatus transferqueue r = do
	failed <- runThreadState st $ getFailedTransfers (Remote.uuid r)
	runThreadState st $ mapM_ removeFailedTransfer $ map fst failed
	mapM_ retry failed
	where
		retry (t, info)
			| transferDirection t == Download = do
				{- Check if the remote still has the key.
				 - If not, relies on the expensiveScan to
				 - get it queued from some other remote. -}
				whenM (runThreadState st $ remoteHas r $ transferKey t) $
					requeue t info
			| otherwise = do
				{- The Transferrer checks when uploading
				 - that the remote doesn't already have the
				 - key, so it's not redundantly checked
				 - here. -}
				requeue t info
		requeue t info = queueTransferWhenSmall
			transferqueue dstatus (associatedFile info) t r

{- This is a expensive scan through the full git work tree, finding
 - files to download from or upload to any of the remotes.
 - 
 - The scan is blocked when the transfer queue gets too large. -}
expensiveScan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> [Remote] -> IO ()
expensiveScan st dstatus transferqueue rs = unless onlyweb $ do
	liftIO $ debug thisThread ["starting scan of", show visiblers]
	void $ alertWhile dstatus (scanAlert visiblers) $ do
		g <- runThreadState st $ fromRepo id
		(files, cleanup) <- LsFiles.inRepo [] g
		go files
		void cleanup
		return True
	liftIO $ debug thisThread ["finished scan of", show visiblers]
	where
		onlyweb = all (== webUUID) $ map Remote.uuid rs
		visiblers = let rs' = filter (not . Remote.readonly) rs
			in if null rs' then rs else rs'
		go [] = noop
		go (f:fs) = do
			mapM_ (enqueue f) =<< runThreadState st
				(ifAnnexed f (findtransfers f) $ return [])
			go fs
		enqueue f (r, t) = do
			debug thisThread ["queuing", show t]
			queueTransferWhenSmall transferqueue dstatus (Just f) t r
		findtransfers f (key, _) = do
			locs <- loggedLocations key
			let use a = return $ catMaybes $ map (a key locs) rs
			ifM (inAnnex key)
				( filterM (wantSend (Just f) . Remote.uuid . fst)
					=<< use (check Upload False)
				, ifM (wantGet $ Just f)
					( use (check Download True) , return [] )
				)
		check direction want key locs r
			| direction == Upload && Remote.readonly r = Nothing
			| (Remote.uuid r `elem` locs) == want = Just
				(r, Transfer direction (Remote.uuid r) key)
			| otherwise = Nothing

remoteHas :: Remote -> Key -> Annex Bool
remoteHas r key = elem
	<$> pure (Remote.uuid r)
	<*> loggedLocations key
