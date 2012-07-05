{- git-annex assistant pending transfer queue
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferQueue where

import Common.Annex
import Assistant.DaemonStatus
import Logs.Transfer
import Types.Remote
import qualified Remote

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

{- Adds pending transfers to the end of the queue for some of the known
 - remotes. (TBD: a smaller set of remotes that are sufficient to transfer to,
 - rather than transferring to all.) -}
queueTransfers :: TransferQueue -> DaemonStatusHandle -> Key -> AssociatedFile -> Direction -> Annex ()
queueTransfers q daemonstatus k f direction =
	mapM_ (liftIO . queueTransfer q f . gentransfer)
		=<< knownRemotes <$> getDaemonStatus daemonstatus
	where
		gentransfer r = Transfer
			{ transferDirection = direction
			, transferKey = k
			, transferRemote = Remote.uuid r
			}

{- Adds a pending transfer to the end of the queue. -}
queueTransfer :: TransferQueue -> AssociatedFile -> Transfer -> IO ()
queueTransfer q f t = void $ atomically $
	writeTChan q (t, stubInfo f)

{- Adds a pending transfer to the start of the queue, to be processed next. -}
queueNextTransfer :: TransferQueue -> AssociatedFile -> Transfer -> IO ()
queueNextTransfer q f t = void $ atomically $
	unGetTChan q (t, stubInfo f)

{- Blocks until a pending transfer is available in the queue. -}
getNextTransfer :: TransferQueue -> IO (Transfer, TransferInfo)
getNextTransfer = atomically . readTChan
