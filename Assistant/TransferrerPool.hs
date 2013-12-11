{- A pool of "git-annex transferkeys" processes
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferrerPool where

import Assistant.Common
import Assistant.Types.TransferrerPool
import Logs.Transfer
import Utility.Batch

import qualified Command.TransferKeys as T

import Control.Concurrent.STM
import System.Process (create_group, std_in, std_out)
import Control.Exception (throw)
import Control.Concurrent

{- Runs an action with a Transferrer from the pool. -}
withTransferrer :: FilePath -> BatchCommandMaker -> TransferrerPool -> (Transferrer -> IO a) -> IO a
withTransferrer program batchmaker pool a = do
	t <- maybe (mkTransferrer program batchmaker) (checkTransferrer program batchmaker)
		=<< atomically (tryReadTChan pool)
	v <- tryNonAsync $ a t
	unlessM (putback t) $
		void $ forkIO $ stopTransferrer t
	either throw return v
  where
  	putback t = atomically $ ifM (isEmptyTChan pool)
		( do
			writeTChan pool t
			return True
		, return False
		)

{- Requests that a Transferrer perform a Transfer, and waits for it to
 - finish. -}
performTransfer :: Transferrer -> Transfer -> AssociatedFile -> IO Bool
performTransfer transferrer t f = catchBoolIO $ do
	T.sendRequest t f (transferrerWrite transferrer)
	T.readResponse (transferrerRead transferrer)

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
	fileEncoding readh
	fileEncoding writeh
	return $ Transferrer
		{ transferrerRead = readh
		, transferrerWrite = writeh
		, transferrerHandle = pid
		}

{- Checks if a Transferrer is still running. If not, makes a new one. -}
checkTransferrer :: FilePath -> BatchCommandMaker -> Transferrer -> IO Transferrer
checkTransferrer program batchmaker t =
	maybe (return t) (const $ mkTransferrer program batchmaker)
		=<< getProcessExitCode (transferrerHandle t)

{- Closing the fds will stop the transferrer. -}
stopTransferrer :: Transferrer -> IO ()
stopTransferrer t = do
	hClose $ transferrerRead t
	hClose $ transferrerWrite t
	void $ waitForProcess $ transferrerHandle t
