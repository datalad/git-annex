{- git-annex assistant transfer slots
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferSlots where

import Control.Exception
import Control.Concurrent

type TransferSlots = QSemN

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
 - action in the slot. If the action throws an exception, its slot is
 - freed here, otherwise it should be freed by the TransferWatcher when
 - the transfer is complete.
 -}
inTransferSlot :: TransferSlots -> IO a -> IO a
inTransferSlot s a = bracketOnError start abort run
	where
		start = waitQSemN s 1
		abort = const $ transferComplete s
		run = const a

{- Call when a transfer is complete. -}
transferComplete :: TransferSlots -> IO ()
transferComplete s = signalQSemN s 1
