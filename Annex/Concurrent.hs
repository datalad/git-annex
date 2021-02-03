{- git-annex concurrent state
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Concurrent (
	module Annex.Concurrent,
	module Annex.Concurrent.Utility
) where

import Annex
import Annex.Common
import Annex.Concurrent.Utility
import qualified Annex.Queue
import Annex.Action
import Types.Concurrency
import Types.CatFileHandles
import Annex.CheckAttr
import Annex.CheckIgnore

import qualified Data.Map as M

setConcurrency :: ConcurrencySetting -> Annex ()
setConcurrency (ConcurrencyCmdLine s) = setConcurrency' s ConcurrencyCmdLine
setConcurrency (ConcurrencyGitConfig s) = setConcurrency' s ConcurrencyGitConfig

setConcurrency' :: Concurrency -> (Concurrency -> ConcurrencySetting) -> Annex ()
setConcurrency' NonConcurrent f = 
	Annex.changeState $ \s -> s 
		{ Annex.concurrency = f NonConcurrent
		}
setConcurrency' c f = do
	cfh <- getState Annex.catfilehandles
	cfh' <- case cfh of
		CatFileHandlesNonConcurrent _ -> liftIO catFileHandlesPool
		CatFileHandlesPool _ -> pure cfh
	cah <- mkConcurrentCheckAttrHandle c
	cih <- mkConcurrentCheckIgnoreHandle c
	Annex.changeState $ \s -> s
		{ Annex.concurrency = f c
		, Annex.catfilehandles = cfh'
		, Annex.checkattrhandle = Just cah
		, Annex.checkignorehandle = Just cih
		}

{- Allows forking off a thread that uses a copy of the current AnnexState
 - to run an Annex action.
 -
 - The returned IO action can be used to start the thread.
 - It returns an Annex action that must be run in the original 
 - calling context to merge the forked AnnexState back into the
 - current AnnexState.
 -}
forkState :: Annex a -> Annex (IO (Annex a))
forkState a = do
	st <- dupState
	return $ do
		(ret, newst) <- run st a
		return $ do
			mergeState newst
			return ret

{- Returns a copy of the current AnnexState that is safe to be
 - used when forking off a thread. 
 -
 - After an Annex action is run using this AnnexState, it
 - should be merged back into the current Annex's state,
 - by calling mergeState.
 -}
dupState :: Annex AnnexState
dupState = do
	st <- Annex.getState id
	-- Make sure that concurrency is enabled, if it was not already,
	-- so the concurrency-safe resource pools are set up.
	st' <- case getConcurrency' (Annex.concurrency st) of
		NonConcurrent -> do
			setConcurrency (ConcurrencyCmdLine (Concurrent 1))
			Annex.getState id
		_ -> return st
	return $ st'
		-- each thread has its own repoqueue
		{ Annex.repoqueue = Nothing
		-- no errors from this thread yet
		, Annex.errcounter = 0
		}

{- Merges the passed AnnexState into the current Annex state.
 - Also closes various handles in it. -}
mergeState :: AnnexState -> Annex ()
mergeState st = do
	st' <- liftIO $ snd <$> run st stopNonConcurrentSafeCoProcesses
	forM_ (M.toList $ Annex.cleanupactions st') $
		uncurry addCleanupAction
	Annex.Queue.mergeFrom st'
	changeState $ \s -> s { errcounter = errcounter s + errcounter st' }
