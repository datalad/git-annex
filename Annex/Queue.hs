{- git-annex command queue
 -
 - Copyright 2011, 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.Queue (
	addCommand,
	addInternalAction,
	addUpdateIndex,
	flush,
	flushWhenFull,
	size,
	get,
	mergeFrom,
) where

import Annex.Common
import Annex hiding (new)
import qualified Git.Queue
import qualified Git.UpdateIndex

import qualified Control.Concurrent.SSem as SSem

{- Adds a git command to the queue. -}
addCommand :: String -> [CommandParam] -> [FilePath] -> Annex ()
addCommand command params files = do
	q <- get
	store <=< flushWhenFull <=< inRepo $
		Git.Queue.addCommand command params files q

addInternalAction :: Git.Queue.InternalActionRunner -> [(FilePath, IO Bool)] -> Annex ()
addInternalAction runner files = do
	q <- get
	store <=< flushWhenFull <=< inRepo $
		Git.Queue.addInternalAction runner files q

{- Adds an update-index stream to the queue. -}
addUpdateIndex :: Git.UpdateIndex.Streamer -> Annex ()
addUpdateIndex streamer = do
	q <- get
	store <=< flushWhenFull <=< inRepo $
		Git.Queue.addUpdateIndex streamer q

{- Runs the queue if it is full. -}
flushWhenFull :: Git.Queue.Queue -> Annex Git.Queue.Queue
flushWhenFull q
	| Git.Queue.full q = flush' q
	| otherwise = return q

{- Runs (and empties) the queue. -}
flush :: Annex ()
flush = do
	q <- get
	unless (0 == Git.Queue.size q) $ do
		store =<< flush' q

{- When there are multiple worker threads, each has its own queue.
 -
 - But, flushing two queues at the same time could lead to failures due to
 - git locking files. So, only one queue is allowed to flush at a time.
 - The repoqueuesem is shared between threads.
 -}
flush' :: Git.Queue.Queue -> Annex Git.Queue.Queue
flush' q = bracket lock unlock go
  where
	lock = do
		s <- getState repoqueuesem
		liftIO $ SSem.wait s
		return s
	unlock = liftIO . SSem.signal
	go _ = do
		showStoringStateAction
		inRepo $ Git.Queue.flush q

{- Gets the size of the queue. -}
size :: Annex Int
size = Git.Queue.size <$> get

get :: Annex Git.Queue.Queue
get = maybe new return =<< getState repoqueue

new :: Annex Git.Queue.Queue
new = do
	q <- Git.Queue.new . annexQueueSize <$> getGitConfig
	store q
	return q

store :: Git.Queue.Queue -> Annex ()
store q = changeState $ \s -> s { repoqueue = Just q }

mergeFrom :: AnnexState -> Annex ()
mergeFrom st = case repoqueue st of
	Nothing -> noop
	Just newq -> do
		q <- get
		let !q' = Git.Queue.merge q newq
		store =<< flushWhenFull q'
