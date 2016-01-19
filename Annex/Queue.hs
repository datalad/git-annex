{- git-annex command queue
 -
 - Copyright 2011, 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.Queue (
	addCommand,
	addUpdateIndex,
	flush,
	flushWhenFull,
	size,
	mergeFrom,
) where

import Common.Annex
import Annex hiding (new)
import qualified Git.Queue
import qualified Git.UpdateIndex

{- Adds a git command to the queue. -}
addCommand :: String -> [CommandParam] -> [FilePath] -> Annex ()
addCommand command params files = do
	q <- get
	store <=< flushWhenFull <=< inRepo $
		Git.Queue.addCommand command params files q

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

flush' :: Git.Queue.Queue -> Annex Git.Queue.Queue
flush' q = do
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
