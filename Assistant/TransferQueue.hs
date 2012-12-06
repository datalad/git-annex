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

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Types.TransferQueue
import Logs.Transfer
import Types.Remote
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Wanted

import Control.Concurrent.STM
import qualified Data.Map as M

{- Reads the queue's content without blocking or changing it. -}
getTransferQueue :: Assistant [(Transfer, TransferInfo)]
getTransferQueue = (atomically . readTVar . queuelist) <<~ transferQueue

stubInfo :: AssociatedFile -> Remote -> TransferInfo
stubInfo f r = stubTransferInfo
	{ transferRemote = Just r
	, associatedFile = f
	}

{- Adds transfers to queue for some of the known remotes.
 - Honors preferred content settings, only transferring wanted files. -}
queueTransfers :: Schedule -> Key -> AssociatedFile -> Direction -> Assistant ()
queueTransfers = queueTransfersMatching (const True)

{- Adds transfers to queue for some of the known remotes, that match a
 - condition. Honors preferred content settings. -}
queueTransfersMatching :: (UUID -> Bool) -> Schedule -> Key -> AssociatedFile -> Direction -> Assistant ()
queueTransfersMatching matching schedule k f direction
	| direction == Download = whenM (liftAnnex $ wantGet True f) go
	| otherwise = go
  where
	go = do
		rs <- liftAnnex . sufficientremotes
			=<< syncDataRemotes <$> getDaemonStatus
		let matchingrs = filter (matching . Remote.uuid) rs
		if null matchingrs
			then defer
			else forM_ matchingrs $ \r ->
				enqueue schedule (gentransfer r) (stubInfo f r)
	sufficientremotes rs
		{- Queue downloads from all remotes that
		 - have the key, with the cheapest ones first.
		 - More expensive ones will only be tried if
		 - downloading from a cheap one fails. -}
		| direction == Download = do
			uuids <- Remote.keyLocations k
			return $ filter (\r -> uuid r `elem` uuids) rs
		{- Upload to all remotes that want the content. -}
		| otherwise = filterM (wantSend True f . Remote.uuid) $
			filter (not . Remote.readonly) rs
	gentransfer r = Transfer
		{ transferDirection = direction
		, transferKey = k
		, transferUUID = Remote.uuid r
		}
	defer
		{- Defer this download, as no known remote has the key. -}
		| direction == Download = do
			q <- getAssistant transferQueue
			void $ liftIO $ atomically $
				modifyTVar' (deferreddownloads q) $
					\l -> (k, f):l
		| otherwise = noop

{- Queues any deferred downloads that can now be accomplished, leaving
 - any others in the list to try again later. -}
queueDeferredDownloads :: Schedule -> Assistant ()
queueDeferredDownloads schedule = do
	q <- getAssistant transferQueue
	l <- liftIO $ atomically $ swapTVar (deferreddownloads q) []
	rs <- syncDataRemotes <$> getDaemonStatus
	left <- filterM (queue rs) l
	unless (null left) $
		liftIO $ atomically $ modifyTVar' (deferreddownloads q) $
			\new -> new ++ left
  where
	queue rs (k, f) = do
		uuids <- liftAnnex $ Remote.keyLocations k
		let sources = filter (\r -> uuid r `elem` uuids) rs
		unless (null sources) $
			forM_ sources $ \r ->
				enqueue schedule (gentransfer r) (stubInfo f r)
		return $ null sources
	  where
		gentransfer r = Transfer
			{ transferDirection = Download
			, transferKey = k
			, transferUUID = Remote.uuid r
			}

enqueue :: Schedule -> Transfer -> TransferInfo -> Assistant ()
enqueue schedule t info
	| schedule == Next = go (new:)
	| otherwise = go (\l -> l++[new])
  where
	new = (t, info)
	go modlist = do
		q <- getAssistant transferQueue
		liftIO $ atomically $ do
			void $ modifyTVar' (queuesize q) succ
			void $ modifyTVar' (queuelist q) modlist
		notifyTransfer

{- Adds a transfer to the queue. -}
queueTransfer :: Schedule -> AssociatedFile -> Transfer -> Remote -> Assistant ()
queueTransfer schedule f t remote = enqueue schedule t (stubInfo f remote)

{- Blocks until the queue is no larger than a given size, and then adds a
 - transfer to the queue. -}
queueTransferAt :: Int -> Schedule -> AssociatedFile -> Transfer -> Remote -> Assistant ()
queueTransferAt wantsz schedule f t remote = do
	q <- getAssistant transferQueue
	liftIO $ atomically $ do
		sz <- readTVar (queuesize q)
		unless (sz <= wantsz) $
			retry -- blocks until queuesize changes
	enqueue schedule t (stubInfo f remote)

queueTransferWhenSmall :: AssociatedFile -> Transfer -> Remote -> Assistant ()
queueTransferWhenSmall = queueTransferAt 10 Later

{- Blocks until a pending transfer is available in the queue,
 - and removes it.
 -
 - Checks that it's acceptable, before adding it to the
 - the currentTransfers map. If it's not acceptable, it's discarded.
 -
 - This is done in a single STM transaction, so there is no window
 - where an observer sees an inconsistent status. -}
getNextTransfer :: (TransferInfo -> Bool) -> Assistant (Maybe (Transfer, TransferInfo))
getNextTransfer acceptable = do
	q <- getAssistant transferQueue
	dstatus <- getAssistant daemonStatusHandle
	liftIO $ atomically $ do
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
getMatchingTransfers :: (Transfer -> Bool) -> Assistant [(Transfer, TransferInfo)]
getMatchingTransfers c = do
	q <- getAssistant transferQueue
	dstatus <- getAssistant daemonStatusHandle
	liftIO $ atomically $ do
		ts <- dequeueTransfersSTM q c
		unless (null ts) $
			adjustTransfersSTM dstatus $ \m -> M.union m $ M.fromList ts
		return ts

{- Removes transfers matching a condition from the queue, and returns the
 - removed transfers. -}
dequeueTransfers :: (Transfer -> Bool) -> Assistant [(Transfer, TransferInfo)]
dequeueTransfers c = do
	q <- getAssistant transferQueue
	removed <- liftIO $ atomically $ dequeueTransfersSTM q c
	unless (null removed) $
		notifyTransfer
	return removed

dequeueTransfersSTM :: TransferQueue -> (Transfer -> Bool) -> STM [(Transfer, TransferInfo)]
dequeueTransfersSTM q c = do
	(removed, ts) <- partition (c . fst)
		<$> readTVar (queuelist q)
	void $ writeTVar (queuesize q) (length ts)
	void $ writeTVar (queuelist q) ts
	return removed
