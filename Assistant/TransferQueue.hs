{- git-annex assistant pending transfer queue
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.TransferQueue (
	TransferQueue,
	Schedule(..),
	newTransferQueue,
	getTransferQueue,
	queueTransfers,
	queueTransfer,
	queueTransferAt,
	getNextTransfer
) where

import Common.Annex
import Assistant.DaemonStatus
import Logs.Transfer
import Types.Remote
import qualified Remote

import Control.Concurrent.STM

{- The transfer queue consists of a channel listing the transfers to make;
 - the size of the queue is also tracked, and a list is maintained
 - in parallel to allow for reading. -}
data TransferQueue = TransferQueue
	{ queue :: TChan (Transfer, TransferInfo)
	, queuesize :: TVar Integer
	, queuelist :: TVar [(Transfer, TransferInfo)]
	}

data Schedule = Next | Later
	deriving (Eq)

newTransferQueue :: IO TransferQueue
newTransferQueue = atomically $ TransferQueue
	<$> newTChan
	<*> newTVar 0
	<*> newTVar []

{- Reads the queue's content without blocking or changing it. -}
getTransferQueue :: TransferQueue -> IO [(Transfer, TransferInfo)]
getTransferQueue q = atomically $ readTVar $ queuelist q

stubInfo :: AssociatedFile -> Remote -> TransferInfo
stubInfo f r = TransferInfo
	{ startedTime = Nothing
	, transferPid = Nothing
	, transferTid = Nothing
	, transferRemote = Just r
	, bytesComplete = Nothing
	, associatedFile = f
	}

{- Adds transfers to queue for some of the known remotes. -}
queueTransfers :: Schedule -> TransferQueue -> DaemonStatusHandle -> Key -> AssociatedFile -> Direction -> Annex ()
queueTransfers schedule q daemonstatus k f direction = do
	rs <- knownRemotes <$> getDaemonStatus daemonstatus
	mapM_ go =<< sufficientremotes rs
	where
		sufficientremotes rs
			-- Queue downloads from all remotes that
			-- have the key, with the cheapest ones first.
			-- More expensive ones will only be tried if
			-- downloading from a cheap one fails.
			| direction == Download = do
				uuids <- Remote.keyLocations k
				return $ filter (\r -> uuid r `elem` uuids) rs
			-- TODO: Determine a smaller set of remotes that
			-- can be uploaded to, in order to ensure all
			-- remotes can access the content. Currently,
			-- send to every remote we can.
			| otherwise = return rs
		gentransfer r = Transfer
			{ transferDirection = direction
			, transferKey = k
			, transferUUID = Remote.uuid r
			}
		go r = liftIO $ atomically $
			enqueue schedule q (gentransfer r) (stubInfo f r)

enqueue :: Schedule -> TransferQueue -> Transfer -> TransferInfo -> STM ()
enqueue schedule q t info
	| schedule == Next = go unGetTChan (new:)
	| otherwise = go writeTChan (\l -> l++[new])
	where
		new = (t, info)
		go modqueue modlist = do
			void $ modqueue (queue q) new
			void $ modifyTVar' (queuesize q) succ
			void $ modifyTVar' (queuelist q) modlist

{- Adds a transfer to the queue. -}
queueTransfer :: Schedule -> TransferQueue -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransfer schedule q f t remote = atomically $
	enqueue schedule q t (stubInfo f remote)

{- Blocks until the queue is no larger than a given size, and then adds a
 - transfer to the queue. -}
queueTransferAt :: Integer -> Schedule -> TransferQueue -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransferAt wantsz schedule q f t remote = atomically $ do
	sz <- readTVar (queuesize q)
	if sz <= wantsz
		then enqueue schedule q t (stubInfo f remote)
		else retry -- blocks until queuesize changes

{- Blocks until a pending transfer is available from the queue. -}
getNextTransfer :: TransferQueue -> IO (Transfer, TransferInfo)
getNextTransfer q = atomically $ do
	void $ modifyTVar' (queuesize q) pred
	void $ modifyTVar' (queuelist q) (drop 1)
	readTChan (queue q)
