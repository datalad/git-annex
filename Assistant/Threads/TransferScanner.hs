{- git-annex assistant thread to scan remotes to find needed transfers
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferScanner where

import Assistant.Common
import Assistant.Types.ScanRemotes
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.Drop
import Logs.Transfer
import Logs.Location
import Logs.Web (webUUID)
import qualified Remote
import qualified Types.Remote as Remote
import Utility.ThreadScheduler
import qualified Git.LsFiles as LsFiles
import qualified Backend
import Annex.Content
import Annex.Wanted

import qualified Data.Set as S

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -}
transferScannerThread :: NamedThread
transferScannerThread = NamedThread "TransferScanner" $ do
	startupScan
	go S.empty
  where
	go scanned = do
		liftIO $ threadDelaySeconds (Seconds 2)
		(rs, infos) <- unzip <$> getScanRemote
		if any fullScan infos || any (`S.notMember` scanned) rs
			then do
				expensiveScan rs
				go $ scanned `S.union` S.fromList rs
			else do
				mapM_ failedTransferScan rs
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
	startupScan = addScanRemotes True =<< syncDataRemotes <$> getDaemonStatus

{- This is a cheap scan for failed transfers involving a remote. -}
failedTransferScan :: Remote -> Assistant ()
failedTransferScan r = do
	failed <- liftAnnex $ getFailedTransfers (Remote.uuid r)
	liftAnnex $ mapM_ removeFailedTransfer $ map fst failed
	mapM_ retry failed
  where
	retry (t, info)
		| transferDirection t == Download = do
			{- Check if the remote still has the key.
			 - If not, relies on the expensiveScan to
			 - get it queued from some other remote. -}
			whenM (liftAnnex $ remoteHas r $ transferKey t) $
				requeue t info
		| otherwise = do
			{- The Transferrer checks when uploading
			 - that the remote doesn't already have the
			 - key, so it's not redundantly checked here. -}
			requeue t info
	requeue t info = queueTransferWhenSmall (associatedFile info) t r

{- This is a expensive scan through the full git work tree, finding
 - files to transfer. The scan is blocked when the transfer queue gets
 - too large. 
 -
 - This also finds files that are present either here or on a remote
 - but that are not preferred content, and drops them. Searching for files
 - to drop is done concurrently with the scan for transfers.
 -
 - TODO: It would be better to first drop as much as we can, before
 - transferring much, to minimise disk use.
 -}
expensiveScan :: [Remote] -> Assistant ()
expensiveScan rs = unless onlyweb $ do
	debug ["starting scan of", show visiblers]
	void $ alertWhile (scanAlert visiblers) $ do
		g <- liftAnnex gitRepo
		(files, cleanup) <- liftIO $ LsFiles.inRepo [] g
		forM_ files $ \f -> do
			ts <- maybe (return []) (findtransfers f)
				=<< liftAnnex (Backend.lookupFile f)
			mapM_ (enqueue f) ts
		void $ liftIO cleanup
		return True
	debug ["finished scan of", show visiblers]
  where
	onlyweb = all (== webUUID) $ map Remote.uuid rs
	visiblers = let rs' = filter (not . Remote.readonly) rs
		in if null rs' then rs else rs'
	enqueue f (r, t) = do
		debug ["queuing", show t]
		queueTransferWhenSmall (Just f) t r
	findtransfers f (key, _) = do
		{- The syncable remotes may have changed since this
		 - scan began. -}
		syncrs <- syncDataRemotes <$> getDaemonStatus
		liftAnnex $ do
			locs <- loggedLocations key
			present <- inAnnex key

			handleDropsFrom locs syncrs present key (Just f) Nothing

			let slocs = S.fromList locs
			let use a = return $ catMaybes $ map (a key slocs) syncrs
			if present
				then filterM (wantSend True (Just f) . Remote.uuid . fst)
					=<< use (genTransfer Upload False)
				else ifM (wantGet True $ Just f)
					( use (genTransfer Download True) , return [] )

genTransfer :: Direction -> Bool -> Key -> S.Set UUID -> Remote -> Maybe (Remote, Transfer)
genTransfer direction want key slocs r
	| direction == Upload && Remote.readonly r = Nothing
	| (S.member (Remote.uuid r) slocs) == want = Just
		(r, Transfer direction (Remote.uuid r) key)
	| otherwise = Nothing

remoteHas :: Remote -> Key -> Annex Bool
remoteHas r key = elem
	<$> pure (Remote.uuid r)
	<*> loggedLocations key
