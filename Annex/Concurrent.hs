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
 -}
enteringStage :: WorkerStage -> Annex a -> Annex a
enteringStage newstage a = Annex.getState Annex.workers >>= \case
	Nothing -> a
	Just tv -> do
		mytid <- liftIO myThreadId
		let set = changeStageTo mytid tv newstage
		let restore = maybe noop (void . changeStageTo mytid tv)
		bracket set restore (const a)

{- This needs to leave the WorkerPool with the same number of
 - idle and active threads, and with the same number of threads for each
 - WorkerStage. So, all it can do is swap the WorkerStage of our thread's
 - ActiveWorker with an IdleWorker.
 -
 - Must avoid a deadlock if all worker threads end up here at the same
 - time, or if there are no suitable IdleWorkers left. So if necessary
 - we first replace our ActiveWorker with an IdleWorker in the pool, to allow
 - some other thread to use it, before waiting for a suitable IdleWorker
 - for us to use.
 -
 - Note that the spareVals in the WorkerPool does not get anything added to
 - it when adding the IdleWorker, so there will for a while be more IdleWorkers
 - in the pool than spareVals. That does not prevent other threads that call
 - this from using them though, so it's fine.
 -}
changeStageTo :: ThreadId -> TMVar (WorkerPool AnnexState) -> WorkerStage -> Annex (Maybe WorkerStage)
changeStageTo mytid tv newstage = liftIO $
	replaceidle >>= maybe
		(return Nothing)
		(either waitidle (return . Just))
  where
	replaceidle = atomically $ do
		pool <- takeTMVar tv
		let notchanging = do
			putTMVar tv pool
			return Nothing
		if memberStage newstage (usedStages pool)
			then case removeThreadIdWorkerPool mytid pool of
				Just ((myaid, oldstage), pool')
					| oldstage /= newstage -> case getIdleWorkerSlot newstage pool' of
						Nothing -> do
							putTMVar tv $
								addWorkerPool (IdleWorker oldstage) pool'
							return $ Just $ Left (myaid, oldstage)
						Just pool'' -> do
							-- optimisation
							putTMVar tv $
								addWorkerPool (IdleWorker oldstage) $
									addWorkerPool (ActiveWorker myaid newstage) pool''
							return $ Just $ Right oldstage
					| otherwise -> notchanging
				_ -> notchanging
			else notchanging
	
	waitidle (myaid, oldstage) = atomically $ do
		pool <- waitIdleWorkerSlot newstage =<< takeTMVar tv
		putTMVar tv $ addWorkerPool (ActiveWorker myaid newstage) pool
		return (Just oldstage)

-- | Waits until there's an idle worker in the worker pool
-- for its initial stage, removes it from the pool, and returns its state.
--
-- If the worker pool is not already allocated, returns Nothing.
waitInitialWorkerSlot :: TMVar (WorkerPool Annex.AnnexState) -> STM (Maybe (Annex.AnnexState, WorkerStage))
waitInitialWorkerSlot tv = do
	pool <- takeTMVar tv
	let stage = initialStage (usedStages pool)
	st <- go stage pool
	return $ Just (st, stage)
  where
	go wantstage pool = case spareVals pool of
		[] -> retry
		(v:vs) -> do
			let pool' = pool { spareVals = vs }
			putTMVar tv =<< waitIdleWorkerSlot wantstage pool'
			return v

waitIdleWorkerSlot :: WorkerStage -> WorkerPool Annex.AnnexState -> STM (WorkerPool Annex.AnnexState)
waitIdleWorkerSlot wantstage = maybe retry return . getIdleWorkerSlot wantstage

getIdleWorkerSlot :: WorkerStage -> WorkerPool Annex.AnnexState -> Maybe (WorkerPool Annex.AnnexState)
getIdleWorkerSlot wantstage pool = do
	l <- findidle [] (workerList pool)
	return $ pool { workerList = l }
  where
	findidle _ [] = Nothing
	findidle c ((IdleWorker stage):rest)
		| stage == wantstage = Just (c ++ rest)
	findidle c (w:rest) = findidle (w:c) rest
