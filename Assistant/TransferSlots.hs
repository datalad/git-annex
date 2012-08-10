{- git-annex assistant transfer slots
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Assistant.TransferSlots where

import qualified Control.Exception as E
import Control.Concurrent
import Data.Typeable

import Common.Annex
import Utility.ThreadScheduler

type TransferSlots = QSemN

{- A special exception that can be thrown to pause or resume a transfer, while
 - keeping its slot in use. -}
data TransferException = PauseTransfer | ResumeTransfer
	deriving (Show, Eq, Typeable)

instance E.Exception TransferException

{- Number of concurrent transfers allowed to be run from the assistant.
 -
 - Transfers launched by other means, including by remote assistants,
 - do not currently take up slots.
 -}
numSlots :: Int
numSlots = 1

newTransferSlots :: IO TransferSlots
newTransferSlots = newQSemN numSlots

{- Waits until a transfer slot becomes available, and runs a transfer
 - action in the slot, in its own thread. 
 -
 - Note that the action is subject to being killed when the transfer
 - is canceled or paused.
 -
 - A PauseTransfer exception is handled by letting the action be killed,
 - then pausing the thread until a ResumeTransfer exception is raised,
 - then rerunning the action.
 -}
inTransferSlot :: TransferSlots -> IO () -> IO ThreadId
inTransferSlot s transfer = do
	waitQSemN s 1
	forkIO $ E.bracket_ noop (signalQSemN s 1) go
	where
		go = catchPauseResume transfer
		pause = catchPauseResume $ runEvery (Seconds 86400) noop
		catchPauseResume a = E.catch a handlePauseResume
		handlePauseResume PauseTransfer = do
			putStrLn "pause"
			pause
		handlePauseResume ResumeTransfer = do
			putStrLn "resume"
			go
