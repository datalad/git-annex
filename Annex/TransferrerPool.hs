{- A pool of "git-annex transferkeys" processes
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Annex.TransferrerPool where

import Annex.Common
import Types.TransferrerPool
import Types.Transfer
import Utility.Batch
import Messages.Serialized
import qualified Command.TransferKeys as T

import Control.Concurrent.STM hiding (check)
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)

{- Runs an action with a Transferrer from the pool.
 -
 - When minimizeprocesses is True, only one Transferrer is left running
 - in the pool at a time. So if this needed to start a new Transferrer,
 - it's stopped when done. Otherwise, idle processes are left in the pool
 - for use later.
 -}
withTransferrer :: Bool -> MkCheckTransferrer -> FilePath -> BatchCommandMaker -> TransferrerPool -> (Transferrer -> IO a) -> IO a
withTransferrer minimizeprocesses mkcheck program batchmaker pool a = do
	(mi, leftinpool) <- atomically (popTransferrerPool pool)
	i@(TransferrerPoolItem (Just t) check) <- case mi of
		Nothing -> mkTransferrerPoolItem mkcheck =<< mkTransferrer program batchmaker
		Just i -> checkTransferrerPoolItem program batchmaker i
	a t `finally` returntopool leftinpool check t i
  where
	returntopool leftinpool check t i
		| not minimizeprocesses || leftinpool == 0 =
			atomically $ pushTransferrerPool pool i
		| otherwise = do
			void $ forkIO $ stopTransferrer t
			atomically $ pushTransferrerPool pool $ TransferrerPoolItem Nothing check

{- Check if a Transferrer from the pool is still ok to be used.
 - If not, stop it and start a new one. -}
checkTransferrerPoolItem :: FilePath -> BatchCommandMaker -> TransferrerPoolItem -> IO TransferrerPoolItem
checkTransferrerPoolItem program batchmaker i = case i of
	TransferrerPoolItem (Just t) check -> ifM check
		( return i
		, do
			stopTransferrer t
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
	-> Transfer
	-> TransferInfo
	-> (forall a. Annex a -> m a)
	-- ^ Run an annex action in the monad. Will not be used with
	-- actions that block for a long time.
	-> m Bool
performTransfer transferrer t info runannex = catchBoolIO $ do
	(liftIO $ T.sendRequest t info (transferrerWrite transferrer))
	relaySerializedOutput
		(liftIO $ T.readResponse (transferrerRead transferrer))
		(liftIO . T.sendSerializedOutputResponse (transferrerWrite transferrer))
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

{- Closing the fds will stop the transferrer, but only when it's in between
 - transfers. -}
stopTransferrer :: Transferrer -> IO ()
stopTransferrer t = do
	hClose $ transferrerRead t
	hClose $ transferrerWrite t
	void $ waitForProcess $ transferrerHandle t
