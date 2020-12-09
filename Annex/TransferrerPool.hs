{- A pool of "git-annex transfer" processes
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Annex.TransferrerPool where

import Annex.Common
import qualified Annex
import Types.TransferrerPool
import Types.Transfer
import Types.Key
import qualified Types.Remote as Remote
import Git.Types (RemoteName)
import Types.StallDetection
import Types.Messages
import Messages.Serialized
import Annex.Path
import Utility.Batch
import Utility.Metered
import Utility.HumanTime
import Utility.ThreadScheduler

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM hiding (check)
import Control.Monad.IO.Class (MonadIO)
import Text.Read (readMaybe)
import Data.Time.Clock.POSIX
import System.Log.Logger (debugM)

data TransferRequest = TransferRequest TransferRequestLevel Direction (Either UUID RemoteName) KeyData AssociatedFile
	deriving (Show, Read)

data TransferRequestLevel = AnnexLevel | AssistantLevel
	deriving (Show, Read)

data TransferResponse
	= TransferOutput SerializedOutput
	| TransferResult Bool
	deriving (Show, Read)

{- Runs an action with a Transferrer from the pool. -}
withTransferrer :: (Transferrer -> Annex a) -> Annex a
withTransferrer a = do
	program <- liftIO programPath
	pool <- Annex.getState Annex.transferrerpool
	let nocheck = pure (pure True)
	withTransferrer' False nocheck program nonBatchCommandMaker pool a

withTransferrer'
	:: (MonadIO m, MonadFail m, MonadMask m)
	=> Bool
	-- ^ When minimizeprocesses is True, only one Transferrer is left
	-- running in the pool at a time. So if this needed to start a
	-- new Transferrer, it's stopped when done. Otherwise, idle
	-- processes are left in the pool for use later.
	-> MkCheckTransferrer
	-> FilePath
	-> BatchCommandMaker
	-> TransferrerPool
	-> (Transferrer -> m a)
	-> m a
withTransferrer' minimizeprocesses mkcheck program batchmaker pool a = do
	(mi, leftinpool) <- liftIO $ atomically (popTransferrerPool pool)
	i@(TransferrerPoolItem (Just t) check) <- liftIO $ case mi of
		Nothing -> mkTransferrerPoolItem mkcheck =<< mkTransferrer program batchmaker
		Just i -> checkTransferrerPoolItem program batchmaker i
	a t `finally` returntopool leftinpool check t i
  where
	returntopool leftinpool check t i
		| not minimizeprocesses || leftinpool == 0 =
			-- If the transferrer got killed, the handles will
			-- be closed, so it should not be returned to the
			-- pool.
			liftIO $ whenM (hIsOpen (transferrerWrite t)) $
				liftIO $ atomically $ pushTransferrerPool pool i
		| otherwise = liftIO $ do
			void $ forkIO $ shutdownTransferrer t
			atomically $ pushTransferrerPool pool $ TransferrerPoolItem Nothing check

{- Check if a Transferrer from the pool is still ok to be used.
 - If not, stop it and start a new one. -}
checkTransferrerPoolItem :: FilePath -> BatchCommandMaker -> TransferrerPoolItem -> IO TransferrerPoolItem
checkTransferrerPoolItem program batchmaker i = case i of
	TransferrerPoolItem (Just t) check -> ifM check
		( return i
		, do
			shutdownTransferrer t
			new check
		)
	TransferrerPoolItem Nothing check -> new check
  where
	new check = do
		t <- mkTransferrer program batchmaker
		return $ TransferrerPoolItem (Just t) check

{- Requests that a Transferrer perform a Transfer, and waits for it to
 - finish.
 -
 - When a stall is detected, kills the Transferrer.
 -
 - If the transfer failed or stalled, returns TransferInfo with an
 - updated bytesComplete reflecting how much data has been transferred.
 -}
performTransfer
	:: (Monad m, MonadIO m, MonadMask m)
	=> Maybe StallDetection
	-> TransferRequestLevel
	-> (forall a. Annex a -> m a)
	-- ^ Run an annex action in the monad. Will not be used with
	-- actions that block for a long time.
	-> Maybe Remote
	-> Transfer
	-> TransferInfo
	-> Transferrer
	-> m (Either TransferInfo ())
performTransfer stalldetection level runannex r t info transferrer = do
	bpv <- liftIO $ newTVarIO zeroBytesProcessed
	ifM (catchBoolIO $ bracket setup cleanup (go bpv))
		( return (Right ())
		, do
			n <- case transferDirection t of
				Upload -> liftIO $ atomically $ 
					fromBytesProcessed <$> readTVar bpv
				Download -> do
					f <- runannex $ fromRepo $ gitAnnexTmpObjectLocation (transferKey t)
					liftIO $ catchDefaultIO 0 $ getFileSize f
			return $ Left $ info { bytesComplete = Just n }
		)
  where
	setup = do
		liftIO $ sendRequest level t r
			(associatedFile info)
			(transferrerWrite transferrer)
		metervar <- liftIO $ newEmptyTMVarIO
		stalledvar <- liftIO $ newTVarIO False
		tid <- liftIO $ async $ 
			detectStalls stalldetection metervar $ do
				atomically $ writeTVar stalledvar True
				killTransferrer transferrer
		return (metervar, tid, stalledvar)
	
	cleanup (_, tid, stalledvar) = do
		liftIO $ uninterruptibleCancel tid
		whenM (liftIO $ atomically $ readTVar stalledvar) $ do
			runannex $ showLongNote "Transfer stalled"
			-- Close handles, to prevent the transferrer being
			-- reused since the process was killed.
			liftIO $ hClose $ transferrerRead transferrer
			liftIO $ hClose $ transferrerWrite transferrer

	go bpv (metervar, _, _) = relaySerializedOutput
		(liftIO $ readResponse (transferrerRead transferrer))
		(liftIO . sendSerializedOutputResponse (transferrerWrite transferrer))
		(updatemeter bpv metervar)
		runannex
	
	updatemeter bpv metervar (Just n) = liftIO $ do
		atomically $ do
			void $ tryTakeTMVar metervar
			putTMVar metervar n
		atomically $ writeTVar bpv n
	updatemeter _bpv metervar Nothing = liftIO $
		atomically $ void $ tryTakeTMVar metervar

detectStalls :: Maybe StallDetection -> TMVar BytesProcessed -> IO () -> IO ()
detectStalls Nothing _ _ = noop
detectStalls (Just (StallDetection minsz duration)) metervar onstall = go Nothing
  where
	go st = do
		starttm <- getPOSIXTime
		threadDelaySeconds (Seconds (fromIntegral (durationSeconds duration)))
		-- Get whatever progress value was reported most recently, or
		-- if none were reported since last time, wait until one is
		-- reported.
		sofar <- atomically $ fromBytesProcessed <$> takeTMVar metervar
		case st of
			Nothing -> go (Just sofar)
			Just prev
				-- Just in case a progress meter somehow runs
				-- backwards, or a second progress meter was
				-- started and is at a smaller value than
				-- the previous one.
				| prev > sofar -> go (Just sofar)
				| otherwise -> do
					endtm <- getPOSIXTime
					let actualduration = endtm - starttm
					let sz = sofar - prev
					let expectedsz = (minsz * durationSeconds duration)
						`div` max 1 (ceiling actualduration)
					if sz < expectedsz
						then onstall
						else go (Just sofar)

{- Starts a new git-annex transfer process, setting up handles
 - that will be used to communicate with it. -}
mkTransferrer :: FilePath -> BatchCommandMaker -> IO Transferrer
mkTransferrer program batchmaker = do
	{- It runs as a batch job. -}
	let (program', params') = batchmaker (program, [Param "transfer"])
	{- It's put into its own group so that the whole group can be
	 - killed to stop a transfer. -}
	(Just writeh, Just readh, _, pid) <- createProcess
		(proc program' $ toCommand params')
		{ create_group = True
		, std_in = CreatePipe
		, std_out = CreatePipe
		}
	return $ Transferrer
		{ transferrerRead = readh
		, transferrerWrite = writeh
		, transferrerHandle = pid
		}

-- | Send a request to perform a transfer.
sendRequest :: TransferRequestLevel -> Transfer -> Maybe Remote -> AssociatedFile -> Handle -> IO ()
sendRequest level t mremote afile h = do
	let l = show $ TransferRequest level
		(transferDirection t)
		(maybe (Left (transferUUID t)) (Right . Remote.name) mremote)
		(keyData (transferKey t))
		afile
	debugM "transfer" ("> " ++ l)
	hPutStrLn h l
	hFlush h

sendSerializedOutputResponse :: Handle -> SerializedOutputResponse -> IO ()
sendSerializedOutputResponse h sor = hPutStrLn h $ show sor

-- | Read a response to a transfer requests.
--
-- Before the final response, this will return whatever SerializedOutput
-- should be displayed as the transfer is performed.
readResponse :: Handle -> IO (Either SerializedOutput Bool)
readResponse h = do
	l <- liftIO $ hGetLine h
	debugM "transfer" ("< " ++ l)
	case readMaybe l of
		Just (TransferOutput so) -> return (Left so)
		Just (TransferResult r) -> return (Right r)
		Nothing -> transferProtocolError l

transferProtocolError :: String -> a
transferProtocolError l = error $ "transfer protocol error: " ++ show l

{- Closing the fds will shut down the transferrer, but only when it's
 - in between transfers. -}
shutdownTransferrer :: Transferrer -> IO ()
shutdownTransferrer t = do
	hClose $ transferrerRead t
	hClose $ transferrerWrite t
	void $ waitForProcess $ transferrerHandle t

{- Kill the transferrer, and all its child processes. -}
killTransferrer :: Transferrer -> IO ()
killTransferrer t = do
	interruptProcessGroupOf $ transferrerHandle t
	threadDelay 50000 -- 0.05 second grace period
	terminateProcess $ transferrerHandle t
