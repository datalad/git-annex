{- git-annex command queue
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module AnnexQueue (
	add,
	flush,
	flushWhenFull
) where

import Control.Monad.State (liftIO)
import Control.Monad (when, unless)

import Annex
import Messages
import qualified Git.Queue
import Utility

{- Adds a git command to the queue, possibly running previously queued
 - actions if enough have accumulated. -}
add :: String -> [CommandParam] -> [FilePath] -> Annex ()
add command params files = do
	q <- getState repoqueue
	store $ Git.Queue.add q command params files

{- Runs the queue if it is full. Should be called periodically. -}
flushWhenFull :: Annex ()
flushWhenFull = do
	q <- getState repoqueue
	when (Git.Queue.full q) $ flush False

{- Runs (and empties) the queue. -}
flush :: Bool -> Annex ()
flush silent = do
	q <- getState repoqueue
	unless (0 == Git.Queue.size q) $ do
		unless silent $
			showSideAction "Recording state in git"
		g <- gitRepo
		q' <- liftIO $ Git.Queue.flush g q
		store q'

store :: Git.Queue.Queue -> Annex ()
store q = changeState $ \s -> s { repoqueue = q }
