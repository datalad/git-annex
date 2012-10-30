{- git-annex assistant transfer slots
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferSlots where

import Assistant.Common
import Utility.ThreadScheduler
import Assistant.Types.TransferSlots
import Assistant.DaemonStatus
import Logs.Transfer

import qualified Control.Exception as E
import Control.Concurrent
import qualified Control.Concurrent.MSemN as MSemN

{- Waits until a transfer slot becomes available, then runs a
 - TransferGenerator, and then runs the transfer action in its own thread. 
 -}
inTransferSlot :: TransferSlotRunner
inTransferSlot dstatus s gen = do
	MSemN.wait s 1
	runTransferThread dstatus s =<< gen

{- Runs a TransferGenerator, and its transfer action,
 - without waiting for a slot to become available. -}
inImmediateTransferSlot :: TransferSlotRunner
inImmediateTransferSlot dstatus s gen = do
	MSemN.signal s (-1)
	runTransferThread dstatus s =<< gen

{- Runs a transfer action, in an already allocated transfer slot.
 - Once it finishes, frees the transfer slot.
 -
 - Note that the action is subject to being killed when the transfer
 - is canceled or paused.
 -
 - A PauseTransfer exception is handled by letting the action be killed,
 - then pausing the thread until a ResumeTransfer exception is raised,
 - then rerunning the action.
 -}
runTransferThread :: DaemonStatusHandle -> TransferSlots -> Maybe (Transfer, TransferInfo, IO ()) -> IO ()
runTransferThread _ s  Nothing = MSemN.signal s 1
runTransferThread dstatus s (Just (t, info, a)) = do
	tid <- forkIO go
	updateTransferInfo dstatus t $ info { transferTid = Just tid }
	where
		go = catchPauseResume a
		pause = catchPauseResume $ runEvery (Seconds 86400) noop
		{- Note: This must use E.try, rather than E.catch.
		 - When E.catch is used, and has called go in its exception
		 - handler, Control.Concurrent.throwTo will block sometimes
		 - when signaling. Using E.try avoids the problem. -}
		catchPauseResume a' = do
			r <- E.try a' :: IO (Either E.SomeException ())
			case r of
				Left e -> case E.fromException e of
					Just PauseTransfer -> pause
					Just ResumeTransfer -> go
					_ -> done
				_ -> done
		done = MSemN.signal s 1
