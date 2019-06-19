{- git-annex concurrent state
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Concurrent where

import Annex
import Annex.Common
import qualified Annex.Queue
import Annex.CatFile
import Annex.CheckAttr
import Annex.HashObject
import Annex.CheckIgnore
import Types.WorkerPool

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M

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
	return $ st
		-- each thread has its own repoqueue
		{ Annex.repoqueue = Nothing
		-- avoid sharing eg, open file handles
		, Annex.catfilehandles = M.empty
		, Annex.checkattrhandle = Nothing
		, Annex.checkignorehandle = Nothing
		}

{- Merges the passed AnnexState into the current Annex state.
 - Also closes various handles in it. -}
mergeState :: AnnexState -> Annex ()
mergeState st = do
	st' <- liftIO $ snd <$> run st stopCoProcesses
	forM_ (M.toList $ Annex.cleanup st') $
		uncurry addCleanup
	Annex.Queue.mergeFrom st'
	changeState $ \s -> s { errcounter = errcounter s + errcounter st' }

{- Stops all long-running git query processes. -}
stopCoProcesses :: Annex ()
stopCoProcesses = do
	catFileStop
	checkAttrStop
	hashObjectStop
	checkIgnoreStop

{- Runs an action and makes the current thread have the specified stage
 - while doing so. If too many other threads are running in the specified
 - stage, waits for one of them to become idle.
 -
 - Noop if the current thread already has the requested stage, or if the
 - current thread is not in the worker pool, or if concurrency is not
 - enabled.
 -
 - Also a noop if the stage is not one of the stages that the worker pool
 - uses.
 -
 - The pool needs to continue to contain the same number of worker threads
 - for each stage. So, an idle worker with the desired stage is found in 
 - the pool (waiting if necessary for one to become idle), and the stages
 - of it and the current thread are swapped.
 -}
enteringStage :: WorkerStage -> Annex a -> Annex a
enteringStage newstage a = Annex.getState Annex.workers >>= \case
	Nothing -> a
	Just tv -> do
		mytid <- liftIO myThreadId
		let set = changeStageTo mytid tv newstage
		let restore = maybe noop (void . changeStageTo mytid tv)
		bracket set restore (const a)

changeStageTo :: ThreadId -> TMVar (WorkerPool AnnexState) -> WorkerStage -> Annex (Maybe WorkerStage)
changeStageTo mytid tv newstage = liftIO $ atomically $ do
	pool <- takeTMVar tv
	case pool of
		WorkerPool usedstages _
			| memberStage newstage usedstages ->
				case removeThreadIdWorkerPool mytid pool of
					Just ((myaid, oldstage), WorkerPool usedstages' l)
						| oldstage /= newstage -> do
							(idlest, restpool) <- waitWorkerSlot usedstages' newstage l
							let pool' = addWorkerPool (IdleWorker idlest oldstage) $
								addWorkerPool (ActiveWorker myaid newstage) restpool
							putTMVar tv pool'
							return (Just oldstage)
					_ -> do
						putTMVar tv pool
						return Nothing
		_ -> do
			putTMVar tv pool
			return Nothing

-- | Waits until there's an idle worker in the worker pool
-- for its initial stage, removes it from the pool, and returns its state.
--
-- If the worker pool is not already allocated, returns Nothing.
waitInitialWorkerSlot :: TMVar (WorkerPool Annex.AnnexState) -> STM (Maybe (Annex.AnnexState, WorkerStage))
waitInitialWorkerSlot tv = do
	WorkerPool usedstages l <- takeTMVar tv
	let stage = initialStage usedstages
	(st, pool') <- waitWorkerSlot usedstages stage l
	putTMVar tv pool'
	return $ Just (st, stage)

-- | Waits until there's an idle worker for the specified stage, and returns
-- its state and a WorkerPool containing all the other workers.
waitWorkerSlot :: UsedStages -> WorkerStage -> [Worker Annex.AnnexState] -> STM (Annex.AnnexState, WorkerPool Annex.AnnexState)
waitWorkerSlot usedstages wantstage = findidle []
  where
	findidle _ [] = retry
	findidle c ((IdleWorker st stage):rest) 
		| stage == wantstage = return (st, WorkerPool usedstages (c ++ rest))
	findidle c (w:rest) = findidle (w:c) rest
