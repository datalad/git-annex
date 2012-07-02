{- git-annex assistant pending transfer queue
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferQueue where

import Common.Annex
import Utility.TSet
import Logs.Transfer
import Types.Remote

import Control.Concurrent.STM

type TransferQueue = TChan (Transfer, TransferInfo)

newTransferQueue :: IO TransferQueue
newTransferQueue = atomically newTChan

stubInfo :: AssociatedFile -> TransferInfo
stubInfo f = TransferInfo
	{ startedTime = Nothing
	, transferPid = Nothing
	, transferThread = Nothing
	, bytesComplete = Nothing
	, associatedFile = f
	}

{- Adds a pending transfer to the end of the queue. -}
queueTransfer :: TransferQueue -> Transfer -> AssociatedFile -> IO ()
queueTransfer q transfer f = void $ atomically $
	writeTChan q (transfer, stubInfo f)

{- Adds a pending transfer to the start of the queue, to be processed next. -}
queueNextTransfer :: TransferQueue -> Transfer -> AssociatedFile -> IO ()
queueNextTransfer q transfer f = void $ atomically $
	unGetTChan q (transfer, stubInfo f)

{- Blocks until a pending transfer is available in the queue. -}
getNextTransfer :: TransferQueue -> IO (Transfer, TransferInfo)
getNextTransfer = atomically . readTChan
