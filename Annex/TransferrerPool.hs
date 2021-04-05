{- A pool of "git-annex transferrer" processes
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Annex.TransferrerPool where

import Annex.Common
import qualified Annex
import Types.TransferrerPool
import Types.Transferrer
import Types.Transfer
import qualified Types.Remote as Remote
import Types.Messages
import Types.CleanupActions
import Messages.Serialized
import Annex.Path
import Annex.StallDetection
import Utility.Batch
import Utility.Metered
import Utility.Debug
import qualified Utility.SimpleProtocol as Proto

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM hiding (check)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M
#ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.Posix.Process (getProcessGroupIDOf)
#endif

type SignalActionsVar = TVar (M.Map SignalAction (Int -> IO ()))

data RunTransferrer = RunTransferrer String [CommandParam] BatchCommandMaker

mkRunTransferrer :: BatchCommandMaker -> Annex RunTransferrer
mkRunTransferrer batchmaker = RunTransferrer
	<$> liftIO programPath
	<*> gitAnnexChildProcessParams "transferrer" []
	<*> pure batchmaker

{- Runs an action with a Transferrer from the pool. -}
withTransferrer :: (Transferrer -> Annex a) -> Annex a
withTransferrer a = do
	rt <- mkRunTransferrer nonBatchCommandMaker
	pool <- Annex.getRead Annex.transferrerpool
	let nocheck = pure (pure True)
	signalactonsvar <- Annex.getRead Annex.signalactions
	withTransferrer' False signalactonsvar nocheck rt pool a

withTransferrer'
	:: (MonadIO m, MonadMask m)
	=> Bool
	-- ^ When minimizeprocesses is True, only one Transferrer is left
	-- running in the pool at a time. So if this needed to start a
	-- new Transferrer, it's stopped when done. Otherwise, idle
	-- processes are left in the pool for use later.
	-> SignalActionsVar
	-> MkCheckTransferrer
	-> RunTransferrer
	-> TransferrerPool
	-> (Transferrer -> m a)
	-> m a
withTransferrer' minimizeprocesses signalactonsvar mkcheck rt pool a = do
	(mi, leftinpool) <- liftIO $ atomically (popTransferrerPool pool)
	(i@(TransferrerPoolItem _ check), t) <- liftIO $ case mi of
		Nothing -> do
			t <- mkTransferrer signalactonsvar rt
			i <- mkTransferrerPoolItem mkcheck t
			return (i, t)
		Just i -> checkTransferrerPoolItem signalactonsvar rt i
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
			void $ forkIO $ transferrerShutdown t
			atomically $ pushTransferrerPool pool $ TransferrerPoolItem Nothing check

{- Check if a Transferrer from the pool is still ok to be used.
 - If not, stop it and start a new one. -}
checkTransferrerPoolItem :: SignalActionsVar -> RunTransferrer -> TransferrerPoolItem -> IO (TransferrerPoolItem, Transferrer)
checkTransferrerPoolItem signalactonsvar rt i = case i of
	TransferrerPoolItem (Just t) check -> ifM check
		( return (i, t)
		, do
			transferrerShutdown t
			new check
		)
	TransferrerPoolItem Nothing check -> new check
  where
	new check = do
		t <- mkTransferrer signalactonsvar rt
		return (TransferrerPoolItem (Just t) check, t)

data TransferRequestLevel = AnnexLevel | AssistantLevel
	deriving (Show)

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
		metervar <- liftIO $ newTVarIO Nothing
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
		atomically $ writeTVar metervar (Just n)
		atomically $ writeTVar bpv n
	updatemeter _bpv metervar Nothing = liftIO $
		atomically $ writeTVar metervar Nothing

{- Starts a new git-annex transfer process, setting up handles
 - that will be used to communicate with it. -}
mkTransferrer :: SignalActionsVar -> RunTransferrer -> IO Transferrer
mkTransferrer signalactonsvar (RunTransferrer program params batchmaker) = do
	{- It runs as a batch job. -}
	let (program', params') = batchmaker (program, params)
	{- It's put into its own group so that the whole group can be
	 - killed to stop a transfer. -}
	(Just writeh, Just readh, _, ph) <- createProcess
		(proc program' $ toCommand params')
		{ create_group = True
		, std_in = CreatePipe
		, std_out = CreatePipe
		}

	{- Set up signal propagation, so eg ctrl-c will also interrupt
	 - the processes in the transferrer's process group. 
	 -
	 - There is a race between the process being created and this point.
	 - If a signal is received before this can run, it is not sent to
	 - the transferrer. This leaves the transferrer waiting for the
	 - first message on stdin to tell what to do. If the signal kills
	 - this parent process, the transferrer will then get a sigpipe
	 - and die too. If the signal suspends this parent process,
	 - it's ok to leave the transferrer running, as it's waiting on
	 - the pipe until this process wakes back up.
	 -}
#ifndef mingw32_HOST_OS
	pid <- getPid ph
	unregistersignalprop <- case pid of
		Just p -> getProcessGroupIDOf p >>= \pgrp -> do
			atomically $ modifyTVar' signalactonsvar $ 
				M.insert (PropagateSignalProcessGroup p) $ \sig ->
					signalProcessGroup (fromIntegral sig) pgrp
			return $ atomically $ modifyTVar' signalactonsvar $
				M.delete (PropagateSignalProcessGroup p)
		Nothing -> return noop
#else
	let unregistersignalprop = noop
#endif

	return $ Transferrer
		{ transferrerRead = readh
		, transferrerWrite = writeh
		, transferrerHandle = ph
		, transferrerShutdown = do
			hClose readh
			hClose writeh
			void $ waitForProcess ph
			unregistersignalprop
		}

-- | Send a request to perform a transfer.
sendRequest :: TransferRequestLevel -> Transfer -> Maybe Remote -> AssociatedFile -> Handle -> IO ()
sendRequest level t mremote afile h = do
	let tr = maybe
		(TransferRemoteUUID (transferUUID t))
		(TransferRemoteName . Remote.name)
		mremote
	let f = case (level, transferDirection t) of
		(AnnexLevel, Upload) -> UploadRequest
		(AnnexLevel, Download) -> DownloadRequest
		(AssistantLevel, Upload) -> AssistantUploadRequest
		(AssistantLevel, Download) -> AssistantDownloadRequest
	let r = f tr (transferKey t) (TransferAssociatedFile afile)
	let l = unwords $ Proto.formatMessage r
	debug "Annex.TransferrerPool" ("> " ++ l)
	hPutStrLn h l
	hFlush h

sendSerializedOutputResponse :: Handle -> SerializedOutputResponse -> IO ()
sendSerializedOutputResponse h sor = do
	let l = unwords $ Proto.formatMessage $
		TransferSerializedOutputResponse sor
	debug "Annex.TransferrerPool" ("> " ++ show l)
	hPutStrLn h l
	hFlush h

-- | Read a response to a transfer request.
--
-- Before the final response, this will return whatever SerializedOutput
-- should be displayed as the transfer is performed.
readResponse :: Handle -> IO (Either SerializedOutput Bool)
readResponse h = do
	l <- liftIO $ hGetLine h
	debug "Annex.TransferrerPool" ("< " ++ l)
	case Proto.parseMessage l of
		Just (TransferOutput so) -> return (Left so)
		Just (TransferResult r) -> return (Right r)
		Nothing -> transferrerProtocolError l

transferrerProtocolError :: String -> a
transferrerProtocolError l = giveup $ "transferrer protocol error: " ++ show l

{- Kill the transferrer, and all its child processes. -}
killTransferrer :: Transferrer -> IO ()
killTransferrer t = do
	interruptProcessGroupOf $ transferrerHandle t
	threadDelay 50000 -- 0.05 second grace period
	terminateProcess $ transferrerHandle t

{- Stop all transferrers in the pool. -}
emptyTransferrerPool :: Annex ()
emptyTransferrerPool = do
	poolvar <- Annex.getRead Annex.transferrerpool
	pool <- liftIO $ atomically $ swapTVar poolvar []
	liftIO $ forM_ pool $ \case
		TransferrerPoolItem (Just t) _ -> transferrerShutdown t
		TransferrerPoolItem Nothing _ -> noop
