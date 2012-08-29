{- git-annex assistant transfer slots
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Assistant.TransferSlots where

import Common.Annex
import Utility.ThreadScheduler
import Assistant.DaemonStatus
import Logs.Transfer

import qualified Control.Exception as E
import Control.Concurrent
import Data.Typeable

type TransferSlots = QSemN

{- A special exception that can be thrown to pause or resume a transfer, while
 - keeping its slot in use. -}
data TransferException = PauseTransfer | ResumeTransfer
	deriving (Show, Eq, Typeable)

instance E.Exception TransferException

type TransferSlotRunner = DaemonStatusHandle -> TransferSlots -> TransferGenerator -> IO ()
type TransferGenerator = IO (Maybe (Transfer, TransferInfo, IO ()))

{- Number of concurrent transfers allowed to be run from the assistant.
 -
 - Transfers launched by other means, including by remote assistants,
 - do not currently take up slots.
 -}
numSlots :: Int
numSlots = 1

newTransferSlots :: IO TransferSlots
newTransferSlots = newQSemN numSlots

{- Waits until a transfer slot becomes available, then runs a
 - TransferGenerator, and then runs the transfer action in its own thread. 
 -}
inTransferSlot :: TransferSlotRunner
inTransferSlot dstatus s gen = do
	waitQSemN s 1
	runTransferThread dstatus s =<< gen

{- Runs a TransferGenerator, and its transfer action,
 - without waiting for a slot to become available. -}
inImmediateTransferSlot :: TransferSlotRunner
inImmediateTransferSlot dstatus s gen = do
	signalQSemN s (-1)
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
runTransferThread _ s  Nothing = signalQSemN s 1
runTransferThread dstatus s (Just (t, info, a)) = do
	tid <- forkIO $ E.bracket_ noop (signalQSemN s 1) go
	updateTransferInfo dstatus t $ info { transferTid = Just tid }
	where
		go = catchPauseResume a
		pause = catchPauseResume $ runEvery (Seconds 1) $ print "paused"
		{- Note: This must use E.try, rather than E.catch.
		 - When E.catch is used, and has called go in its exception
		 - handler, Control.Concurrent.throwTo will block sometimes
		 - when signaling. Using E.try avoids the problem. -}
		catchPauseResume a' = do
			r <- E.try a'
			case r of
				Right v -> return v
				Left PauseTransfer -> pause
				Left ResumeTransfer -> go
