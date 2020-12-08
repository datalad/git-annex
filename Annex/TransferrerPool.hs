{- A pool of "git-annex transferkeys" processes
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
import Types.Messages
import Messages.Serialized
import Annex.Path
import Utility.Batch

import Control.Concurrent.STM hiding (check)
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import Text.Read (readMaybe)
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
 - finish. -}
performTransfer
	:: (Monad m, MonadIO m, MonadMask m)
	=> Transferrer
	-> TransferRequestLevel
	-> Transfer
	-> Maybe Remote
	-> AssociatedFile
	-> (forall a. Annex a -> m a)
	-- ^ Run an annex action in the monad. Will not be used with
	-- actions that block for a long time.
	-> m Bool
performTransfer transferrer level t mremote afile runannex = catchBoolIO $ do
	(liftIO $ sendRequest level t mremote afile (transferrerWrite transferrer))
	relaySerializedOutput
		(liftIO $ readResponse (transferrerRead transferrer))
		(liftIO . sendSerializedOutputResponse (transferrerWrite transferrer))
		runannex

{- Starts a new git-annex transferkeys process, setting up handles
 - that will be used to communicate with it. -}
mkTransferrer :: FilePath -> BatchCommandMaker -> IO Transferrer
mkTransferrer program batchmaker = do
	{- It runs as a batch job. -}
	let (program', params') = batchmaker (program, [Param "transferkeys"])
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
		Nothing -> transferKeysProtocolError l

transferKeysProtocolError :: String -> a
transferKeysProtocolError l = error $ "transferkeys protocol error: " ++ show l

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
	hClose $ transferrerRead t
	hClose $ transferrerWrite t
	interruptProcessGroupOf $ transferrerHandle t
	threadDelay 50000 -- 0.05 second grace period
	terminateProcess $ transferrerHandle t
