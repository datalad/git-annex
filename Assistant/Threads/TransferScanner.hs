{- git-annex assistant thread to scan remotes to find needed transfers
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Threads.TransferScanner where

import Assistant.Common
import Assistant.Types.ScanRemotes
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.DaemonStatus
import Assistant.Drop
import Assistant.Sync
import Assistant.DeleteRemote
import Assistant.Types.UrlRenderer
import Types.Transfer
import Logs.Transfer
import Logs.Location
import Logs.Group
import qualified Remote
import qualified Types.Remote as Remote
import Utility.ThreadScheduler
import Utility.NotificationBroadcaster
import Utility.Batch
import qualified Git.LsFiles as LsFiles
import Annex.WorkTree
import Annex.Content
import Annex.Wanted
import CmdLine.Action
import Types.Command

import qualified Data.Set as S
import Control.Concurrent

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -}
transferScannerThread :: UrlRenderer -> NamedThread
transferScannerThread urlrenderer = namedThread "TransferScanner" $ do
	startupScan
	go S.empty
  where
	go scanned = do
		scanrunning False
		liftIO $ threadDelaySeconds (Seconds 2)
		(rs, infos) <- unzip <$> getScanRemote
		scanrunning True
		if any fullScan infos || any (`S.notMember` scanned) rs
			then do
				expensiveScan urlrenderer rs
				go $ scanned `S.union` S.fromList rs
			else do
				mapM_ failedTransferScan rs
				go scanned
	scanrunning b = do
		ds <- modifyDaemonStatus $ \s -> 
			(s { transferScanRunning = b }, s)
		liftIO $ sendNotification $ transferNotifier ds
		
	{- All git remotes are synced, all exports are updated,
	 - and all available remotes are scanned in full on startup,
	 - for multiple reasons, including:
	 -
	 - * This may be the first run, and there may be remotes
	 -   already in place, that need to be synced.
	 - * Changes may have been made last time we run, but remotes were
	 -   not available to be synced with.
	 - * Changes may have been made to remotes while we were down.
	 - * We may have run before, and scanned a remote, but
	 -   only been in a subdirectory of the git remote, and so
	 -   not synced it all.
	 - * We may have run before, and had transfers queued,
	 -   and then the system (or us) crashed, and that info was
	 -   lost.
	 - * A remote may be in the unwanted group, and this is a chance
	 -   to determine if the remote has been emptied.
	 -}
	startupScan = do
		reconnectRemotes =<< syncGitRemotes <$> getDaemonStatus
		addScanRemotes True =<< scannableRemotes

{- This is a cheap scan for failed transfers involving a remote. -}
failedTransferScan :: Remote -> Assistant ()
failedTransferScan r = do
	failed <- liftAnnex $ clearFailedTransfers (Remote.uuid r)
	mapM_ retry failed
  where
	retry (t, info)
		| transferDirection t == Download =
			{- Check if the remote still has the key.
			 - If not, relies on the expensiveScan to
			 - get it queued from some other remote. -}
			whenM (liftAnnex $ remoteHas r $ transferKey t) $
				requeue t info
		| otherwise =
			{- The Transferrer checks when uploading
			 - that the remote doesn't already have the
			 - key, so it's not redundantly checked here. -}
			requeue t info
	requeue t info = queueTransferWhenSmall "retrying failed transfer" (associatedFile info) t r
	
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
 - 
 - During the scan, we'll also check if any unwanted repositories are empty,
 - and can be removed. While unrelated, this is a cheap place to do it,
 - since we need to look at the locations of all keys anyway.
 -}
expensiveScan :: UrlRenderer -> [Remote] -> Assistant ()
expensiveScan urlrenderer rs = batch <~> do
	debug ["starting scan of", show visiblers]

	let us = map Remote.uuid rs

	mapM_ (liftAnnex . clearFailedTransfers) us

	unwantedrs <- liftAnnex $ S.fromList
		<$> filterM inUnwantedGroup us

	g <- liftAnnex gitRepo
	(files, cleanup) <- liftIO $ LsFiles.inRepo [] [] g
	removablers <- scan unwantedrs files
	void $ liftIO cleanup

	debug ["finished scan of", show visiblers]

	remove <- asIO1 $ removableRemote urlrenderer
	liftIO $ mapM_ (void . tryNonAsync . remove) $ S.toList removablers
  where
	visiblers = let rs' = filter (not . Remote.readonly) rs
		in if null rs' then rs else rs'

	scan unwanted [] = return unwanted
	scan unwanted (f:fs) = do
		(unwanted', ts) <- maybe
			(return (unwanted, []))
			(findtransfers f unwanted)
				=<< liftAnnex (lookupKey f)
		mapM_ (enqueue f) ts

		{- Delay for a short time to avoid using too much CPU. -}
		liftIO $ threadDelay $ fromIntegral $ oneSecond `div` 200

		scan unwanted' fs

	enqueue f (r, t) =
		queueTransferWhenSmall "expensive scan found missing object"
			(AssociatedFile (Just f)) t r
	findtransfers f unwanted key = do
		let af = AssociatedFile (Just f)
		locs <- liftAnnex $ loggedLocations key
		present <- liftAnnex $ inAnnex key
		let slocs = S.fromList locs
		
		{- The remotes may have changed since this scan began. -}
		syncrs <- syncDataRemotes <$> getDaemonStatus
		let use l a = mapMaybe (a key slocs) . l <$> getDaemonStatus

		liftAnnex $ handleDropsFrom locs syncrs
			"expensive scan found too many copies of object"
			present key af (SeekInput []) [] callCommandAction
		ts <- if present
			then liftAnnex . filterM (wantSend True (Just key) af . Remote.uuid . fst)
				=<< use syncDataRemotes (genTransfer Upload False)
			else ifM (liftAnnex $ wantGet True (Just key) af)
				( use downloadRemotes (genTransfer Download True) , return [] )
		let unwanted' = S.difference unwanted slocs
		return (unwanted', ts)

-- Both syncDataRemotes and exportRemotes can be scanned.
-- The downloadRemotes list contains both.
scannableRemotes :: Assistant [Remote]
scannableRemotes = downloadRemotes <$> getDaemonStatus

genTransfer :: Direction -> Bool -> Key -> S.Set UUID -> Remote -> Maybe (Remote, Transfer)
genTransfer direction want key slocs r
	| direction == Upload && Remote.readonly r = Nothing
	| S.member (Remote.uuid r) slocs == want = Just
		(r, Transfer direction (Remote.uuid r) (fromKey id key))
	| otherwise = Nothing

remoteHas :: Remote -> Key -> Annex Bool
remoteHas r key = elem
	<$> pure (Remote.uuid r)
	<*> loggedLocations key
