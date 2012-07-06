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
 - action in the slot.
 -}
inTransferSlot :: TransferSlots -> IO a -> IO a
inTransferSlot s = bracket_ (waitQSemN s 1) (signalQSemN s 1)
