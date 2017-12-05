{- git-annex transfers
 -
 - Copyright 2012-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Annex.Transfer (
	module X,
	upload,
	download,
	runTransfer,
	alwaysRunTransfer,
	noRetry,
	forwardRetry,
	pickRemote,
) where

import Annex.Common
import qualified Annex
import Logs.Transfer as X
import Types.Transfer as X
import Annex.Notification as X
import Annex.Perms
import Utility.Metered
import Annex.LockPool
import Types.Key
import qualified Types.Remote as Remote
import Types.Concurrency

import Control.Concurrent
import qualified Data.Map.Strict as M
import Data.Ord

upload :: Observable v => UUID -> Key -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex v) -> NotifyWitness -> Annex v
upload u key f d a _witness = guardHaveUUID u $ 
	runTransfer (Transfer Upload u key) f d a

download :: Observable v => UUID -> Key -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex v) -> NotifyWitness -> Annex v
download u key f d a _witness = guardHaveUUID u $
	runTransfer (Transfer Download u key) f d a

guardHaveUUID :: Observable v => UUID -> Annex v -> Annex v
guardHaveUUID u a
	| u == NoUUID = return observeFailure
	| otherwise = a

{- Runs a transfer action. Creates and locks the lock file while the
 - action is running, and stores info in the transfer information
 - file.
 -
 - If the transfer action returns False, the transfer info is 
 - left in the failedTransferDir.
 -
 - If the transfer is already in progress, returns False.
 -
 - An upload can be run from a read-only filesystem, and in this case
 - no transfer information or lock file is used.
 -}
runTransfer :: Observable v => Transfer -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex v) -> Annex v
runTransfer = runTransfer' False

{- Like runTransfer, but ignores any existing transfer lock file for the
 - transfer, allowing re-running a transfer that is already in progress.
 -
 - Note that this may result in confusing progress meter display in the
 - webapp, if multiple processes are writing to the transfer info file. -}
alwaysRunTransfer :: Observable v => Transfer -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex v) -> Annex v
alwaysRunTransfer = runTransfer' True

runTransfer' :: Observable v => Bool -> Transfer -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex v) -> Annex v
runTransfer' ignorelock t afile shouldretry transferaction = checkSecureHashes t $ do
	info <- liftIO $ startTransferInfo afile
	(meter, tfile, metervar) <- mkProgressUpdater t info
	mode <- annexFileMode
	(lck, inprogress) <- prep tfile mode info
	if inprogress && not ignorelock
		then do
			showNote "transfer already in progress, or unable to take transfer lock"
			return observeFailure
		else do
			v <- retry info metervar $ transferaction meter
			liftIO $ cleanup tfile lck
			if observeBool v
				then removeFailedTransfer t
				else recordFailedTransfer t info
			return v
  where
#ifndef mingw32_HOST_OS
	prep tfile mode info = catchPermissionDenied (const prepfailed) $ do
		let lck = transferLockFile tfile
		createAnnexDirectory $ takeDirectory lck
		tryLockExclusive (Just mode) lck >>= \case
			Nothing -> return (Nothing, True)
			Just lockhandle -> ifM (checkSaneLock lck lockhandle)
				( do
					void $ liftIO $ tryIO $
						writeTransferInfoFile info tfile
					return (Just lockhandle, False)
				, do
					liftIO $ dropLock lockhandle
					return (Nothing, True)
				)
#else
	prep tfile _mode info = catchPermissionDenied (const prepfailed) $ do
		let lck = transferLockFile tfile
		createAnnexDirectory $ takeDirectory lck
		catchMaybeIO (liftIO $ lockExclusive lck) >>= \case
			Nothing -> return (Nothing, False)
			Just Nothing -> return (Nothing, True)
			Just (Just lockhandle) -> do
				void $ liftIO $ tryIO $
					writeTransferInfoFile info tfile
				return (Just lockhandle, False)
#endif
	prepfailed = return (Nothing, False)

	cleanup _ Nothing = noop
	cleanup tfile (Just lockhandle) = do
		let lck = transferLockFile tfile
		void $ tryIO $ removeFile tfile
#ifndef mingw32_HOST_OS
		void $ tryIO $ removeFile lck
		dropLock lockhandle
#else
		{- Windows cannot delete the lockfile until the lock
		 - is closed. So it's possible to race with another
		 - process that takes the lock before it's removed,
		 - so ignore failure to remove.
		 -}
		dropLock lockhandle
		void $ tryIO $ removeFile lck
#endif
	retry oldinfo metervar run = tryNonAsync run >>= \case
		Right b -> return b
		Left e -> do
			warning (show e)
			b <- getbytescomplete metervar
			let newinfo = oldinfo { bytesComplete = Just b }
			if shouldretry oldinfo newinfo
				then retry newinfo metervar run
				else return observeFailure
	getbytescomplete metervar
		| transferDirection t == Upload =
			liftIO $ readMVar metervar
		| otherwise = do
			f <- fromRepo $ gitAnnexTmpObjectLocation (transferKey t)
			liftIO $ catchDefaultIO 0 $ getFileSize f

{- Avoid download and upload of keys with insecure content when
 - annex.securehashesonly is configured.
 -
 - This is not a security check. Even if this let the content be
 - downloaded, the actual security checks would prevent the content from
 - being added to the repository. The only reason this is done here is to
 - avoid transferring content that's going to be rejected anyway.
 -
 - We assume that, if annex.securehashesonly is set and the local repo
 - still contains content using an insecure hash, remotes will likewise
 - tend to be configured to reject it, so Upload is also prevented.
 -}
checkSecureHashes :: Observable v => Transfer -> Annex v -> Annex v
checkSecureHashes t a
	| cryptographicallySecure variety = a
	| otherwise = ifM (annexSecureHashesOnly <$> Annex.getGitConfig)
		( do
			warning $ "annex.securehashesonly blocked transfer of " ++ formatKeyVariety variety ++ " key"
			return observeFailure
		, a
		)
  where
	variety = keyVariety (transferKey t)

type RetryDecider = TransferInfo -> TransferInfo -> Bool

noRetry :: RetryDecider
noRetry _ _ = False

{- Retries a transfer when it fails, as long as the failed transfer managed
 - to send some data. -}
forwardRetry :: RetryDecider
forwardRetry old new = bytesComplete old < bytesComplete new

{- Picks a remote from the list and tries a transfer to it. If the transfer
 - does not succeed, goes on to try other remotes from the list.
 -
 - The list should already be ordered by remote cost, and is normally
 - tried in order. However, when concurrent jobs are running, they will
 - be assigned different remotes of the same cost when possible. This can
 - increase total transfer speed.
 -}
pickRemote :: Observable v => [Remote] -> (Remote -> Annex v) -> Annex v
pickRemote l a = go l =<< Annex.getState Annex.concurrency
  where
	go [] _ = return observeFailure
	go (r:[]) _ = a r
	go rs (Concurrent n) | n > 1 = do
		mv <- Annex.getState Annex.activeremotes
		active <- liftIO $ takeMVar mv
		let rs' = sortBy (lessActiveFirst active) rs
		goconcurrent mv active rs'
	go (r:rs) _ = do
		ok <- a r
		if observeBool ok
			then return ok
			else go rs NonConcurrent
	goconcurrent mv active [] = do
		liftIO $ putMVar mv active
		return observeFailure
	goconcurrent mv active (r:rs) = do
		let !active' = M.insertWith (+) r 1 active
		liftIO $ putMVar mv active'
		let getnewactive = do
			active'' <- liftIO $ takeMVar mv
			let !active''' = M.update (\n -> if n > 1 then Just (n-1) else Nothing) r active''
			return active'''
		let removeactive = liftIO . putMVar mv =<< getnewactive
		ok <- a r `onException` removeactive
		if observeBool ok
			then do
				removeactive
				return ok 
			else do
				active'' <- getnewactive
				-- Re-sort the remaining rs 
				-- because other threads could have
				-- been assigned them in the meantime.
				let rs' = sortBy (lessActiveFirst active'') rs
				goconcurrent mv active'' rs'

lessActiveFirst :: M.Map Remote Integer -> Remote -> Remote -> Ordering
lessActiveFirst active a b
	| Remote.cost a == Remote.cost b = comparing (`M.lookup` active) a b
	| otherwise = compare a b
