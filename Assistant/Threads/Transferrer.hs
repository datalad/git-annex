{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Assistant.Common
import Assistant.TransferQueue
import Assistant.TransferSlots
import Types.Transfer
import Annex.Path
import Utility.Batch

{- Dispatches transfers from the queue. -}
transfererThread :: NamedThread
transfererThread = namedThread "Transferrer" $ do
	program <- liftIO programPath
	batchmaker <- liftIO getBatchCommandMaker
	forever $ inTransferSlot program batchmaker $
		maybe (return Nothing) (uncurry genTransfer)
			=<< getNextTransfer notrunning
  where
	{- Skip transfers that are already running. -}
	notrunning = isNothing . startedTime
