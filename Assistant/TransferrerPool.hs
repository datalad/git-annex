{- A pool of "git-annex transferkeys" processes
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.TransferrerPool where

import Assistant.Common
import Assistant.Types.TransferrerPool
import Logs.Transfer

#ifndef mingw32_HOST_OS
import qualified Command.TransferKeys as T
#endif

import Control.Concurrent.STM
import System.Process (create_group)
import Control.Exception (throw)
import Control.Concurrent

{- Runs an action with a Transferrer from the pool. -}
withTransferrer :: FilePath -> TransferrerPool -> (Transferrer -> IO a) -> IO a
withTransferrer program pool a = do
	t <- maybe (mkTransferrer program) (checkTransferrer program)
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
#ifndef mingw32_HOST_OS
	T.sendRequest t f (transferrerWrite transferrer)
	T.readResponse (transferrerRead transferrer)
#else
	error "TODO performTransfer not implemented on Windows"
#endif

{- Starts a new git-annex transferkeys process, setting up a pipe
 - that will be used to communicate with it. -}
mkTransferrer :: FilePath -> IO Transferrer
mkTransferrer program = do
#ifndef mingw32_HOST_OS
	(myread, twrite) <- createPipe
	(tread, mywrite) <- createPipe
	mapM_ (\fd -> setFdOption fd CloseOnExec True) [myread, mywrite]
	let params =
		[ Param "transferkeys"
		, Param "--readfd", Param $ show tread
		, Param "--writefd", Param $ show twrite
		]
	{- It's put into its own group so that the whole group can be
	 - killed to stop a transfer. -}
	(_, _, _, pid) <- createProcess (proc program $ toCommand params)
		{ create_group = True }
	closeFd twrite
	closeFd tread
	myreadh <- fdToHandle myread
	mywriteh <- fdToHandle mywrite
	fileEncoding myreadh
	fileEncoding mywriteh
	return $ Transferrer
		{ transferrerRead = myreadh
		, transferrerWrite = mywriteh
		, transferrerHandle = pid
		}
#else
	error "TODO mkTransferrer not implemented on Windows"
#endif

{- Checks if a Transferrer is still running. If not, makes a new one. -}
checkTransferrer :: FilePath -> Transferrer -> IO Transferrer
checkTransferrer program t = maybe (return t) (const $ mkTransferrer program)
	=<< getProcessExitCode (transferrerHandle t)

{- Closing the fds will stop the transferrer. -}
stopTransferrer :: Transferrer -> IO ()
stopTransferrer t = do
	hClose $ transferrerRead t
	hClose $ transferrerWrite t
	void $ waitForProcess $ transferrerHandle t
