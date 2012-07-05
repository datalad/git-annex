{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Logs.Transfer
import Annex.Content
import Annex.BranchState
import Command
import qualified Command.Move

import Control.Exception as E
import Control.Concurrent
import Data.Time.Clock
import qualified Data.Map as M

{- Dispatches transfers from the queue.
 -
 - This is currently very simplistic, and runs only one transfer at a time.
 -}
transfererThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> IO ()
transfererThread st dstatus transferqueue = do
	mypid <- getProcessID
	mytid <- myThreadId
	go mypid mytid
	where
		go mypid mytid = do
			(t, info) <- getNextTransfer transferqueue

			now <- getCurrentTime
			let info' = info
				{ startedTime = Just now
				, transferPid = Just mypid
				, transferThread = Just mytid
				}

			ifM (runThreadState st $ shouldtransfer t info')
				( runTransfer st t info'
				, noop
				)
			go mypid mytid

		-- Check if the transfer is already running,
		-- and if not, add it to the TransferMap.
		shouldtransfer t info = do
			current <- currentTransfers <$> getDaemonStatus dstatus
			if M.member t current
				then ifM (validtransfer t)
					( do
						adjustTransfers dstatus $
							M.insertWith' const t info
						return True
					, return False
					)
				else return False

		validtransfer t
			| transferDirection t == Download =
				not <$> inAnnex (transferKey t)
			| otherwise = return True

{- A transfer is run in a separate thread, with a *copy* of the Annex
 - state. This is necessary to avoid blocking the rest of the assistant
 - on the transfer completing, and also to allow multiple transfers to run
 - at once. 
 -
 - However, it means that the transfer threads are responsible
 - for doing any necessary shutdown cleanups, and that the parent
 - thread's cache must be invalidated, as changes may have been made to the
 - git-annex branch. 
 -
 - Currently a minimal shutdown is done; the transfer threads are
 - effectively running in oneshot mode, without committing changes to the
 - git-annex branch, and transfers should never queue git commands to run.
 -
 - Note: It is unsafe to call getDaemonStatus inside the transfer thread.
 -}
runTransfer :: ThreadState -> Transfer -> TransferInfo -> IO ()
runTransfer st t info
	| transferDirection t == Download = go Command.Move.fromStart
	| otherwise = go Command.Move.toStart
	where
		go cmd = case (transferRemote info, associatedFile info) of
			(Nothing, _) -> noop
			(_, Nothing) -> noop
			(Just remote, Just file) -> 
				inthread $ void $ doCommand $
					cmd remote False file (transferKey t)
		inthread a = do
			mvar <- newEmptyMVar
			void $ forkIO $
				runThreadState st a `E.finally` putMVar mvar ()
			void $ takeMVar mvar -- wait for transfer thread
			runThreadState st invalidateCache
