{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Alert
import Assistant.Alert.Utility
import Assistant.Commits
import Assistant.Drop
import Assistant.TransferrerPool
import Logs.Transfer
import Logs.Location
import Annex.Content
import qualified Remote
import qualified Types.Remote as Remote
import qualified Git
import Config.Files
import Assistant.Threads.TransferWatcher
import Annex.Wanted

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
