{- git-annex assistant pending transfer queue
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

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
import Types.Transfer
import Logs.Transfer
import Types.Remote
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Wanted
import Utility.TList

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S

type Reason = String

{- Reads the queue's content without blocking or changing it. -}
getTransferQueue :: Assistant [(Transfer, TransferInfo)]
getTransferQueue = (atomically . readTList . queuelist) <<~ transferQueue

stubInfo :: AssociatedFile -> Remote -> TransferInfo
stubInfo f r = stubTransferInfo
	{ transferRemote = Just r
	, associatedFile = f
	}

{- Adds transfers to queue for some of the known remotes.
 - Honors preferred content settings, only transferring wanted files. -}
queueTransfers :: Reason -> Schedule -> Key -> AssociatedFile -> Direction -> Assistant Bool
queueTransfers = queueTransfersMatching (const True)

{- Adds transfers to queue for some of the known remotes, that match a
 - condition. Honors preferred content settings. -}
queueTransfersMatching :: (UUID -> Bool) -> Reason -> Schedule -> Key -> AssociatedFile -> Direction -> Assistant Bool
queueTransfersMatching matching reason schedule k f direction
	| direction == Download = ifM (liftAnnex $ wantGet True (Just k) f)
		( go
		, return False
		)
	| otherwise = go
  where
	go = do
		
		rs <- liftAnnex . selectremotes
			=<< syncDataRemotes <$> getDaemonStatus
		let matchingrs = filter (matching . Remote.uuid) rs
		if null matchingrs
			then do
				defer
				return False
			else do
				forM_ matchingrs $ \r ->
					enqueue reason schedule (gentransfer r) (stubInfo f r)
				return True
	selectremotes rs
		{- Queue downloads from all remotes that
		 - have the key. The list of remotes is ordered with
		 - cheapest first. More expensive ones will only be tried
		 - if downloading from a cheap one fails. -}
		| direction == Download = do
			s <- locs
			return $ filter (inset s) rs
		{- Upload to all remotes that want the content and don't
		 - already have it. -}
		| otherwise = do
			s <- locs
			filterM (wantSend True (Just k) f . Remote.uuid) $
				filter (\r -> not (inset s r || Remote.readonly r)) rs
	  where
		locs = S.fromList <$> Remote.keyLocations k
		inset s r = S.member (Remote.uuid r) s
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
				consTList (deferreddownloads q) (k, f)
		| otherwise = noop

{- Queues any deferred downloads that can now be accomplished, leaving
 - any others in the list to try again later. -}
queueDeferredDownloads :: Reason -> Schedule -> Assistant ()
queueDeferredDownloads reason schedule = do
	q <- getAssistant transferQueue
	l <- liftIO $ atomically $ readTList (deferreddownloads q)
	rs <- syncDataRemotes <$> getDaemonStatus
	left <- filterM (queue rs) l
	unless (null left) $
		liftIO $ atomically $ appendTList (deferreddownloads q) left
  where
	queue rs (k, f) = do
		uuids <- liftAnnex $ Remote.keyLocations k
		let sources = filter (\r -> uuid r `elem` uuids) rs
		unless (null sources) $
			forM_ sources $ \r ->
				enqueue reason schedule 
					(gentransfer r) (stubInfo f r)
		return $ null sources
	  where
		gentransfer r = Transfer
			{ transferDirection = Download
			, transferKey = k
			, transferUUID = Remote.uuid r
			}

enqueue :: Reason -> Schedule -> Transfer -> TransferInfo -> Assistant ()
enqueue reason schedule t info
	| schedule == Next = go consTList
	| otherwise = go snocTList
  where
	go modlist = whenM (add modlist) $ do
		debug [ "queued", describeTransfer t info, ": " ++ reason ]
		notifyTransfer
	add modlist = do
		q <- getAssistant transferQueue
		dstatus <- getAssistant daemonStatusHandle
		liftIO $ atomically $ ifM (checkRunningTransferSTM dstatus t)
			( return False
			, do
				l <- readTList (queuelist q)
				if (t `notElem` map fst l)
					then do	
						void $ modifyTVar' (queuesize q) succ
						void $ modlist (queuelist q) (t, info)
						return True
					else return False
			)

{- Adds a transfer to the queue. -}
queueTransfer :: Reason -> Schedule -> AssociatedFile -> Transfer -> Remote -> Assistant ()
queueTransfer reason schedule f t remote =
	enqueue reason schedule t (stubInfo f remote)

{- Blocks until the queue is no larger than a given size, and then adds a
 - transfer to the queue. -}
queueTransferAt :: Int -> Reason -> Schedule -> AssociatedFile -> Transfer -> Remote -> Assistant ()
queueTransferAt wantsz reason schedule f t remote = do
	q <- getAssistant transferQueue
	liftIO $ atomically $ do
		sz <- readTVar (queuesize q)
		unless (sz <= wantsz) $
			retry -- blocks until queuesize changes
	enqueue reason schedule t (stubInfo f remote)

queueTransferWhenSmall :: Reason -> AssociatedFile -> Transfer -> Remote -> Assistant ()
queueTransferWhenSmall reason = queueTransferAt 10 reason Later

{- Blocks until a pending transfer is available in the queue,
 - and removes it.
 -
 - Checks that it's acceptable, before adding it to the
 - currentTransfers map. If it's not acceptable, it's discarded.
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
				(r@(t,info):rest) <- readTList (queuelist q)
				void $ modifyTVar' (queuesize q) pred
				setTList (queuelist q) rest
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
	!(removed, ts) <- partition (c . fst) <$> readTList (queuelist q)
	let !len = length ts
	void $ writeTVar (queuesize q) len
	setTList (queuelist q) ts
	return removed
