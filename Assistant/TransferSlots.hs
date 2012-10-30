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
import Assistant.Types.DaemonStatus
import Logs.Transfer

import qualified Control.Exception as E
import Control.Concurrent
import qualified Control.Concurrent.MSemN as MSemN
import Data.Typeable

type TransferSlots = MSemN.MSemN Int

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
newTransferSlots = MSemN.new numSlots

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
