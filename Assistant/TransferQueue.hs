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
	queueTransfersMatching,
	queueDeferredDownloads,
	queueTransfer,
	queueTransferAt,
	queueTransferWhenSmall,
	getNextTransfer,
	getMatchingTransfers,
	dequeueTransfers,
) where

import Common.Annex
import Assistant.DaemonStatus
import Logs.Transfer
import Types.Remote
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Wanted

import Control.Concurrent.STM
import qualified Data.Map as M

data TransferQueue = TransferQueue
	{ queuesize :: TVar Int
	, queuelist :: TVar [(Transfer, TransferInfo)]
	, deferreddownloads :: TVar [(Key, AssociatedFile)]
	}

data Schedule = Next | Later
	deriving (Eq)

newTransferQueue :: IO TransferQueue
newTransferQueue = atomically $ TransferQueue
	<$> newTVar 0
	<*> newTVar []
	<*> newTVar []

{- Reads the queue's content without blocking or changing it. -}
getTransferQueue :: TransferQueue -> IO [(Transfer, TransferInfo)]
getTransferQueue q = atomically $ readTVar $ queuelist q

stubInfo :: AssociatedFile -> Remote -> TransferInfo
stubInfo f r = stubTransferInfo
	{ transferRemote = Just r
	, associatedFile = f
	}

{- Adds transfers to queue for some of the known remotes.
 - Honors preferred content settings, only transferring wanted files. -}
queueTransfers :: Schedule -> TransferQueue -> DaemonStatusHandle -> Key -> AssociatedFile -> Direction -> Annex ()
queueTransfers = queueTransfersMatching (const True)

{- Adds transfers to queue for some of the known remotes, that match a
 - condition. Honors preferred content settings. -}
queueTransfersMatching :: (UUID -> Bool) -> Schedule -> TransferQueue -> DaemonStatusHandle -> Key -> AssociatedFile -> Direction -> Annex ()
queueTransfersMatching matching schedule q dstatus k f direction
	| direction == Download = whenM (wantGet f) go
	| otherwise = go
	where
		go = do
			rs <- sufficientremotes
				=<< knownRemotes <$> liftIO (getDaemonStatus dstatus)
			let matchingrs = filter (matching . Remote.uuid) rs
			if null matchingrs
				then defer
				else forM_ matchingrs $ \r -> liftIO $
					enqueue schedule q dstatus (gentransfer r) (stubInfo f r)
		sufficientremotes rs
			{- Queue downloads from all remotes that
			 - have the key, with the cheapest ones first.
			 - More expensive ones will only be tried if
			 - downloading from a cheap one fails. -}
			| direction == Download = do
				uuids <- Remote.keyLocations k
				return $ filter (\r -> uuid r `elem` uuids) rs
			{- Upload to all remotes that want the content. -}
			| otherwise = filterM (wantSend f . Remote.uuid) $
				filter (not . Remote.readonly) rs
		gentransfer r = Transfer
			{ transferDirection = direction
			, transferKey = k
			, transferUUID = Remote.uuid r
			}
		defer
			{- Defer this download, as no known remote has the key. -}
			| direction == Download = void $ liftIO $ atomically $
					modifyTVar' (deferreddownloads q) $
						\l -> (k, f):l
			| otherwise = noop

{- Queues any deferred downloads that can now be accomplished, leaving
 - any others in the list to try again later. -}
queueDeferredDownloads :: Schedule -> TransferQueue -> DaemonStatusHandle -> Annex ()
queueDeferredDownloads schedule q dstatus = do
	rs <- knownRemotes <$> liftIO (getDaemonStatus dstatus)
	l <- liftIO $ atomically $ swapTVar (deferreddownloads q) []
	left <- filterM (queue rs) l
	unless (null left) $
		liftIO $ atomically $ modifyTVar' (deferreddownloads q) $
			\new -> new ++ left
	where
		queue rs (k, f) = do
			uuids <- Remote.keyLocations k
			let sources = filter (\r -> uuid r `elem` uuids) rs
			unless (null sources) $
				forM_ sources $ \r -> liftIO $
					enqueue schedule q dstatus
						(gentransfer r) (stubInfo f r)
			return $ null sources
			where
				gentransfer r = Transfer
					{ transferDirection = Download
					, transferKey = k
					, transferUUID = Remote.uuid r
					}

enqueue :: Schedule -> TransferQueue -> DaemonStatusHandle -> Transfer -> TransferInfo -> IO ()
enqueue schedule q dstatus t info
	| schedule == Next = go (new:)
	| otherwise = go (\l -> l++[new])
	where
		new = (t, info)
		go modlist = do
			atomically $ do
				void $ modifyTVar' (queuesize q) succ
				void $ modifyTVar' (queuelist q) modlist
			void $ notifyTransfer dstatus

{- Adds a transfer to the queue. -}
queueTransfer :: Schedule -> TransferQueue -> DaemonStatusHandle -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransfer schedule q dstatus f t remote =
	enqueue schedule q dstatus t (stubInfo f remote)

{- Blocks until the queue is no larger than a given size, and then adds a
 - transfer to the queue. -}
queueTransferAt :: Int -> Schedule -> TransferQueue -> DaemonStatusHandle -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransferAt wantsz schedule q dstatus f t remote = do
	atomically $ do
		sz <- readTVar (queuesize q)
		unless (sz <= wantsz) $
			retry -- blocks until queuesize changes
	enqueue schedule q dstatus t (stubInfo f remote)

queueTransferWhenSmall :: TransferQueue -> DaemonStatusHandle -> AssociatedFile -> Transfer -> Remote -> IO ()
queueTransferWhenSmall = queueTransferAt 10 Later

{- Blocks until a pending transfer is available in the queue,
 - and removes it.
 -
 - Checks that it's acceptable, before adding it to the
 - the currentTransfers map. If it's not acceptable, it's discarded.
 -
 - This is done in a single STM transaction, so there is no window
 - where an observer sees an inconsistent status. -}
getNextTransfer :: TransferQueue -> DaemonStatusHandle -> (TransferInfo -> Bool) -> IO (Maybe (Transfer, TransferInfo))
getNextTransfer q dstatus acceptable = atomically $ do
	sz <- readTVar (queuesize q)
	if sz < 1
		then retry -- blocks until queuesize changes
		else do
			(r@(t,info):rest) <- readTVar (queuelist q)
			writeTVar (queuelist q) rest
			void $ modifyTVar' (queuesize q) pred
			if acceptable info
				then do
					adjustTransfersSTM dstatus $
						M.insertWith' const t info
					return $ Just r
				else return Nothing

{- Moves transfers matching a condition from the queue, to the
 - currentTransfers map. -}
getMatchingTransfers :: TransferQueue -> DaemonStatusHandle -> (Transfer -> Bool) -> IO [(Transfer, TransferInfo)]
getMatchingTransfers q dstatus c = atomically $ do
	ts <- dequeueTransfersSTM q c
	unless (null ts) $
		adjustTransfersSTM dstatus $ \m -> M.union m $ M.fromList ts
	return ts

{- Removes transfers matching a condition from the queue, and returns the
 - removed transfers. -}
dequeueTransfers :: TransferQueue -> DaemonStatusHandle -> (Transfer -> Bool) -> IO [(Transfer, TransferInfo)]
dequeueTransfers q dstatus c = do
	removed <- atomically $ dequeueTransfersSTM q c
	unless (null removed) $
		notifyTransfer dstatus
	return removed

dequeueTransfersSTM :: TransferQueue -> (Transfer -> Bool) -> STM [(Transfer, TransferInfo)]
dequeueTransfersSTM q c = do
	(removed, ts) <- partition (c . fst)
		<$> readTVar (queuelist q)
	void $ writeTVar (queuesize q) (length ts)
	void $ writeTVar (queuelist q) ts
	return removed
