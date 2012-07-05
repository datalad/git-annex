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
	, transferRemote = Nothing
	, bytesComplete = Nothing
	, associatedFile = f
	}

{- Adds pending transfers to the end of the queue for some of the known
 - remotes. -}
queueTransfers :: TransferQueue -> DaemonStatusHandle -> Key -> AssociatedFile -> Direction -> Annex ()
queueTransfers q daemonstatus k f direction =
	mapM_ (\r -> queue r $ gentransfer r)
		=<< sufficientremotes . knownRemotes
			<$> getDaemonStatus daemonstatus
	where
		sufficientremotes l
			-- Queue downloads from all remotes, with the
			-- cheapest ones first. More expensive ones will
			-- only be tried if downloading from a cheap one
			-- fails.
			-- TODO: avoid downloading from remotes that don't
			-- have the key.
			| direction == Download = l
			-- TODO: Determine a smaller set of remotes that
			-- can be uploaded to, in order to ensure all
			-- remotes can access the content. Currently,
			-- send to every remote we can.
			| otherwise = l
		gentransfer r = Transfer
			{ transferDirection = direction
			, transferKey = k
			, transferUUID = Remote.uuid r
			}
		queue r t = liftIO $ void $ atomically $ do
			let info = (stubInfo f) { transferRemote = Just r }
			writeTChan q (t, info)

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
