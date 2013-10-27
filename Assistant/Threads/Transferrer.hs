{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Assistant.Common
import Assistant.TransferQueue
import Assistant.TransferSlots
import Logs.Transfer
import Config.Files

{- Dispatches transfers from the queue. -}
transfererThread :: NamedThread
transfererThread = namedThread "Transferrer" $ do
	program <- liftIO readProgramFile
	forever $ inTransferSlot program $
		maybe (return Nothing) (uncurry genTransfer)
			=<< getNextTransfer notrunning
  where
	{- Skip transfers that are already running. -}
	notrunning = isNothing . startedTime
