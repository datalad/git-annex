{- git-annex command queue
 -
 - Copyright 2011, 2012, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Annex.LockFile
import qualified Git.Queue
import qualified Git.UpdateIndex

{- Adds a git command to the queue. -}
addCommand :: String -> [CommandParam] -> [FilePath] -> Annex ()
addCommand command params files = do
	q <- get
	store =<< flushWhenFull =<<
		(Git.Queue.addCommand command params files q =<< gitRepo)

addInternalAction :: Git.Queue.InternalActionRunner Annex -> [(FilePath, IO Bool)] -> Annex ()
addInternalAction runner files = do
	q <- get
	store =<< flushWhenFull =<<
		(Git.Queue.addInternalAction runner files q =<< gitRepo)

{- Adds an update-index stream to the queue. -}
addUpdateIndex :: Git.UpdateIndex.Streamer -> Annex ()
addUpdateIndex streamer = do
	q <- get
	store =<< flushWhenFull =<<
		(Git.Queue.addUpdateIndex streamer q =<< gitRepo)

{- Runs the queue if it is full. -}
flushWhenFull :: Git.Queue.Queue Annex -> Annex (Git.Queue.Queue Annex)
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
 - And of course multiple git-annex processes may be running each with its
 - own queue.
 -
 - But, flushing two queues at the same time could lead to failures due to
 - git locking files. So, only one queue is allowed to flush at a time.
 -}
flush' :: Git.Queue.Queue Annex -> Annex (Git.Queue.Queue Annex)
flush' q = withExclusiveLock gitAnnexGitQueueLock $ do
	showStoringStateAction
	Git.Queue.flush q =<< gitRepo

{- Gets the size of the queue. -}
size :: Annex Int
size = Git.Queue.size <$> get

get :: Annex (Git.Queue.Queue Annex)
get = maybe new return =<< getState repoqueue

new :: Annex (Git.Queue.Queue Annex)
new = do
	q <- Git.Queue.new . annexQueueSize <$> getGitConfig
	store q
	return q

store :: Git.Queue.Queue Annex -> Annex ()
store q = changeState $ \s -> s { repoqueue = Just q }

mergeFrom :: AnnexState -> Annex ()
mergeFrom st = case repoqueue st of
	Nothing -> noop
	Just newq -> do
		q <- get
		let !q' = Git.Queue.merge q newq
		store =<< flushWhenFull q'
