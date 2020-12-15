{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Assistant.Common
import Assistant.TransferQueue
import Assistant.TransferSlots
import Types.Transfer
import Annex.TransferrerPool
import Utility.Batch

{- Dispatches transfers from the queue. -}
transfererThread :: NamedThread
transfererThread = namedThread "Transferrer" $ do
	rt <- liftAnnex . mkRunTransferrer
		=<< liftIO getBatchCommandMaker
	forever $ inTransferSlot rt $
		maybe (return Nothing) (uncurry genTransfer)
			=<< getNextTransfer notrunning
  where
	{- Skip transfers that are already running. -}
	notrunning = isNothing . startedTime
