{- git-annex transfers
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Annex.Transfer (
	module X,
	upload,
	upload',
	alwaysUpload,
	download,
	download',
	runTransfer,
	alwaysRunTransfer,
	noRetry,
	stdRetry,
	pickRemote,
	stallDetection,
) where

import Annex.Common
import qualified Annex
import Logs.Transfer as X
import Types.Transfer as X
import Annex.Notification as X
import Annex.Content
import Annex.Perms
import Annex.Action
import Logs.Location
import Utility.Metered
import Utility.ThreadScheduler
import Annex.LockPool
import Types.Key
import qualified Types.Remote as Remote
import Types.Concurrency
import Annex.Concurrent
import Types.WorkerPool
import Annex.WorkerPool
import Annex.TransferrerPool
import Annex.StallDetection
import Backend (isCryptographicallySecure)
import Types.StallDetection
import qualified Utility.RawFilePath as R

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM hiding (retry)
import qualified Data.Map.Strict as M
import qualified System.FilePath.ByteString as P
import Data.Ord

-- Upload, supporting canceling detected stalls.
upload :: Remote -> Key -> AssociatedFile -> RetryDecider -> NotifyWitness -> Annex Bool
upload r key f d witness = stallDetection r >>= \case
	Nothing -> go (Just ProbeStallDetection)
	Just StallDetectionDisabled -> go Nothing
	Just sd -> runTransferrer sd r key f d Upload witness
  where
 	go sd = upload' (Remote.uuid r) key f sd d (action . Remote.storeKey r key f) witness

-- Upload, not supporting canceling detected stalls
upload' :: Observable v => UUID -> Key -> AssociatedFile -> Maybe StallDetection -> RetryDecider -> (MeterUpdate -> Annex v) -> NotifyWitness -> Annex v
upload' u key f sd d a _witness = guardHaveUUID u $ 
	runTransfer (Transfer Upload u (fromKey id key)) f sd d a

alwaysUpload :: Observable v => UUID -> Key -> AssociatedFile -> Maybe StallDetection -> RetryDecider -> (MeterUpdate -> Annex v) -> NotifyWitness -> Annex v
alwaysUpload u key f sd d a _witness = guardHaveUUID u $ 
	alwaysRunTransfer (Transfer Upload u (fromKey id key)) f sd d a

-- Download, supporting canceling detected stalls.
download :: Remote -> Key -> AssociatedFile -> RetryDecider -> NotifyWitness -> Annex Bool
download r key f d witness = logStatusAfter key $ stallDetection r >>= \case
	Nothing -> go (Just ProbeStallDetection)
	Just StallDetectionDisabled -> go Nothing
	Just sd -> runTransferrer sd r key f d Download witness
  where
	go sd = getViaTmp (Remote.retrievalSecurityPolicy r) (RemoteVerify r) key f $ \dest ->
		download' (Remote.uuid r) key f sd d (go' dest) witness
	go' dest p = verifiedAction $
		Remote.retrieveKeyFile r key f (fromRawFilePath dest) p

-- Download, not supporting canceling detected stalls.
download' :: Observable v => UUID -> Key -> AssociatedFile -> Maybe StallDetection -> RetryDecider -> (MeterUpdate -> Annex v) -> NotifyWitness -> Annex v
download' u key f sd d a _witness = guardHaveUUID u $
	runTransfer (Transfer Download u (fromKey id key)) f sd d a

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
 -
 - Cannot cancel stalls, but when a likely stall is detected, 
 - suggests to the user that they enable stall detection handling.
 -}
runTransfer :: Observable v => Transfer -> AssociatedFile -> Maybe StallDetection -> RetryDecider -> (MeterUpdate -> Annex v) -> Annex v
runTransfer = runTransfer' False

{- Like runTransfer, but ignores any existing transfer lock file for the
 - transfer, allowing re-running a transfer that is already in progress.
 -}
alwaysRunTransfer :: Observable v => Transfer -> AssociatedFile -> Maybe StallDetection -> RetryDecider -> (MeterUpdate -> Annex v) -> Annex v
alwaysRunTransfer = runTransfer' True

runTransfer' :: Observable v => Bool -> Transfer -> AssociatedFile -> Maybe StallDetection -> RetryDecider -> (MeterUpdate -> Annex v) -> Annex v
runTransfer' ignorelock t afile stalldetection retrydecider transferaction =
	enteringStage TransferStage $
		debugLocks $
			preCheckSecureHashes (transferKey t) go
  where
	go = do
		info <- liftIO $ startTransferInfo afile
		(meter, tfile, createtfile, metervar) <- mkProgressUpdater t info
		mode <- annexFileMode
		(lck, inprogress) <- prep tfile createtfile mode
		if inprogress && not ignorelock
			then do
				showNote "transfer already in progress, or unable to take transfer lock"
				return observeFailure
			else do
				v <- retry 0 info metervar $
					detectStallsAndSuggestConfig stalldetection metervar $
						transferaction meter
				liftIO $ cleanup tfile lck
				if observeBool v
					then removeFailedTransfer t
					else recordFailedTransfer t info
				return v
	
	prep :: RawFilePath -> Annex () -> FileMode -> Annex (Maybe LockHandle, Bool)
#ifndef mingw32_HOST_OS
	prep tfile createtfile mode = catchPermissionDenied (const prepfailed) $ do
		let lck = transferLockFile tfile
		createAnnexDirectory $ P.takeDirectory lck
		tryLockExclusive (Just mode) lck >>= \case
			Nothing -> return (Nothing, True)
			Just lockhandle -> ifM (checkSaneLock lck lockhandle)
				( do
					createtfile
					return (Just lockhandle, False)
				, do
					liftIO $ dropLock lockhandle
					return (Nothing, True)
				)
#else
	prep tfile createtfile _mode = catchPermissionDenied (const prepfailed) $ do
		let lck = transferLockFile tfile
		createAnnexDirectory $ P.takeDirectory lck
		catchMaybeIO (liftIO $ lockExclusive lck) >>= \case
			Nothing -> return (Nothing, False)
			Just Nothing -> return (Nothing, True)
			Just (Just lockhandle) -> do
				createtfile
				return (Just lockhandle, False)
#endif
	prepfailed = return (Nothing, False)

	cleanup _ Nothing = noop
	cleanup tfile (Just lockhandle) = do
		let lck = transferLockFile tfile
		void $ tryIO $ R.removeLink tfile
#ifndef mingw32_HOST_OS
		void $ tryIO $ R.removeLink lck
		dropLock lockhandle
#else
		{- Windows cannot delete the lockfile until the lock
		 - is closed. So it's possible to race with another
		 - process that takes the lock before it's removed,
		 - so ignore failure to remove.
		 -}
		dropLock lockhandle
		void $ tryIO $ R.removeLink lck
#endif

	retry numretries oldinfo metervar run =
		tryNonAsync run >>= \case
			Right v
				| observeBool v -> return v
				| otherwise -> checkretry
			Left e -> do
				warning (show e)
				checkretry
	  where
		checkretry = do
			b <- getbytescomplete metervar
			let newinfo = oldinfo { bytesComplete = Just b }
			let !numretries' = succ numretries
			ifM (retrydecider numretries' oldinfo newinfo)
				( retry numretries' newinfo metervar run
				, return observeFailure
				)

	getbytescomplete metervar
		| transferDirection t == Upload =
			liftIO $ maybe 0 fromBytesProcessed 
				<$> readTVarIO metervar
		| otherwise = do
			f <- fromRepo $ gitAnnexTmpObjectLocation (transferKey t)
			liftIO $ catchDefaultIO 0 $ getFileSize f

detectStallsAndSuggestConfig :: Maybe StallDetection -> TVar (Maybe BytesProcessed) -> Annex a -> Annex a
detectStallsAndSuggestConfig Nothing _ a = a
detectStallsAndSuggestConfig sd@(Just _) metervar a = 
	bracket setup cleanup (const a)
  where
	setup = do
		v <- liftIO newEmptyTMVarIO
		sdt <- liftIO $ async $ detectStalls sd metervar $
			void $ atomically $ tryPutTMVar v True
		wt <- liftIO . async =<< forkState (warnonstall v)
		return (v, sdt, wt)
	cleanup (v, sdt, wt) = do
		liftIO $ uninterruptibleCancel sdt
		void $ liftIO $ atomically $ tryPutTMVar v False
		join (liftIO (wait wt))
	warnonstall v = whenM (liftIO (atomically (takeTMVar v))) $
		warning "Transfer seems to have stalled. To restart stalled transfers, configure annex.stalldetection"

{- Runs a transfer using a separate process, which lets detected stalls be
 - canceled. -}
runTransferrer
	:: StallDetection
	-> Remote
	-> Key
	-> AssociatedFile
	-> RetryDecider
	-> Direction
	-> NotifyWitness
	-> Annex Bool
runTransferrer sd r k afile retrydecider direction _witness =
	enteringStage TransferStage $ preCheckSecureHashes k $ do
		info <- liftIO $ startTransferInfo afile
		go 0 info
  where
	go numretries info = 
		withTransferrer (performTransfer (Just sd) AnnexLevel id (Just r) t info) >>= \case
			Right () -> return True
			Left newinfo -> do
				let !numretries' = succ numretries
				ifM (retrydecider numretries' info newinfo)
					( go numretries' newinfo
					, return False
					)
	t = Transfer direction (Remote.uuid r) (fromKey id k)

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
preCheckSecureHashes :: Observable v => Key -> Annex v -> Annex v
preCheckSecureHashes k a = ifM (isCryptographicallySecure k)
	( a
	, ifM (annexSecureHashesOnly <$> Annex.getGitConfig)
		( do
			warning $ "annex.securehashesonly blocked transfer of " ++ decodeBS (formatKeyVariety variety) ++ " key"
			return observeFailure
		, a
		)
	)
  where
	variety = fromKey keyVariety k

type NumRetries = Integer

type RetryDecider = NumRetries -> TransferInfo -> TransferInfo -> Annex Bool

{- Both retry deciders are checked together, so if one chooses to delay,
 - it will always take effect. -}
combineRetryDeciders :: RetryDecider -> RetryDecider -> RetryDecider
combineRetryDeciders a b = \n old new -> do
	ar <- a n old new
	br <- b n old new
	return (ar || br)

noRetry :: RetryDecider
noRetry _ _ _ = pure False

stdRetry :: RetryDecider
stdRetry = combineRetryDeciders forwardRetry configuredRetry

{- Keep retrying failed transfers, as long as forward progress is being
 - made.
 -
 - Up to a point -- while some remotes can resume where the previous
 - transfer left off, and so it would make sense to keep retrying forever,
 - other remotes restart each transfer from the beginning, and so even if
 - forward progress is being made, it's not real progress. So, retry a
 - maximum of 5 times by default.
 -}
forwardRetry :: RetryDecider
forwardRetry numretries old new
	| fromMaybe 0 (bytesComplete old) < fromMaybe 0 (bytesComplete new) =
		(numretries <=) <$> maybe globalretrycfg pure remoteretrycfg
	| otherwise = return False
  where
	globalretrycfg = fromMaybe 5 . annexForwardRetry
		<$> Annex.getGitConfig
	remoteretrycfg = remoteAnnexRetry =<<
		(Remote.gitconfig <$> transferRemote new)

{- Retries a number of times with growing delays in between when enabled
 - by git configuration. -}
configuredRetry :: RetryDecider
configuredRetry numretries _old new = do
	(maxretries, Seconds initretrydelay) <- getcfg $ 
		Remote.gitconfig <$> transferRemote new
	if numretries < maxretries
		then do
			let retrydelay = Seconds (initretrydelay * 2^(numretries-1))
			showSideAction $ "Delaying " ++ show (fromSeconds retrydelay) ++ "s before retrying."
			liftIO $ threadDelaySeconds retrydelay
			return True
		else return False
  where
	globalretrycfg = fromMaybe 0 . annexRetry
		<$> Annex.getGitConfig
	globalretrydelaycfg = fromMaybe (Seconds 1) . annexRetryDelay
		<$> Annex.getGitConfig
	getcfg Nothing = (,) <$> globalretrycfg <*> globalretrydelaycfg
	getcfg (Just gc) = (,)
		<$> maybe globalretrycfg return (remoteAnnexRetry gc)
		<*> maybe globalretrydelaycfg return (remoteAnnexRetryDelay gc)

{- Picks a remote from the list and tries a transfer to it. If the transfer
 - does not succeed, goes on to try other remotes from the list.
 -
 - The list should already be ordered by remote cost, and is normally
 - tried in order. However, when concurrent jobs are running, they will
 - be assigned different remotes of the same cost when possible. This can
 - increase total transfer speed.
 -}
pickRemote :: Observable v => [Remote] -> (Remote -> Annex v) -> Annex v
pickRemote l a = debugLocks $ go l =<< getConcurrency
  where
	go [] _ = return observeFailure
	go (r:[]) _ = a r
	go rs NonConcurrent = gononconcurrent rs
	go rs (Concurrent n)
		| n <= 1 = gononconcurrent rs
		| otherwise = goconcurrent rs
	go rs ConcurrentPerCpu = goconcurrent rs
	
	gononconcurrent [] = return observeFailure
	gononconcurrent (r:rs) = do
		ok <- a r
		if observeBool ok
			then return ok
			else gononconcurrent rs
	
	goconcurrent rs = do
		mv <- Annex.getState Annex.activeremotes
		active <- liftIO $ takeMVar mv
		let rs' = sortBy (lessActiveFirst active) rs
		goconcurrent' mv active rs'

	goconcurrent' mv active [] = do
		liftIO $ putMVar mv active
		return observeFailure
	goconcurrent' mv active (r:rs) = do
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
				goconcurrent' mv active'' rs'

lessActiveFirst :: M.Map Remote Integer -> Remote -> Remote -> Ordering
lessActiveFirst active a b
	| Remote.cost a == Remote.cost b = comparing (`M.lookup` active) a b
	| otherwise = comparing Remote.cost a b

stallDetection :: Remote -> Annex (Maybe StallDetection)
stallDetection r = maybe globalcfg (pure . Just) remotecfg
  where
	globalcfg = annexStallDetection <$> Annex.getGitConfig
	remotecfg = remoteAnnexStallDetection $ Remote.gitconfig r
