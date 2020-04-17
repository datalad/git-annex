{- git-annex worker thread pool
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.WorkerPool where

import Annex
import Annex.Common
import Types.WorkerPool

import Control.Concurrent
import Control.Concurrent.STM

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
		let set = changeStageTo mytid tv (const newstage)
		let restore = maybe noop (void . changeStageTo mytid tv . const)
		bracket set restore (const a)

{- Transition the current thread to the initial stage.
 - This is done once the thread is ready to begin work.
 -}
enteringInitialStage :: Annex ()
enteringInitialStage = Annex.getState Annex.workers >>= \case
	Nothing -> noop
	Just tv -> do
		mytid <- liftIO myThreadId
		void $ changeStageTo mytid tv initialStage

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
changeStageTo :: ThreadId -> TMVar (WorkerPool AnnexState) -> (UsedStages -> WorkerStage) -> Annex (Maybe WorkerStage)
changeStageTo mytid tv getnewstage = liftIO $
	replaceidle >>= maybe
		(return Nothing)
		(either waitidle (return . Just))
  where
	replaceidle = atomically $ do
		pool <- takeTMVar tv
		let newstage = getnewstage (usedStages pool)
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
							return $ Just $ Left (myaid, newstage, oldstage)
						Just pool'' -> do
							-- optimisation
							putTMVar tv $
								addWorkerPool (IdleWorker oldstage) $
									addWorkerPool (ActiveWorker myaid newstage) pool''
							return $ Just $ Right oldstage
					| otherwise -> notchanging
				_ -> notchanging
			else notchanging
	
	waitidle (myaid, newstage, oldstage) = atomically $ do
		pool <- waitIdleWorkerSlot newstage =<< takeTMVar tv
		putTMVar tv $ addWorkerPool (ActiveWorker myaid newstage) pool
		return (Just oldstage)

-- | Waits until there's an idle StartStage worker in the worker pool,
-- removes it from the pool, and returns its state.
--
-- If the worker pool is not already allocated, returns Nothing.
waitStartWorkerSlot :: TMVar (WorkerPool Annex.AnnexState) -> STM (Maybe (Annex.AnnexState, WorkerStage))
waitStartWorkerSlot tv = do
	pool <- takeTMVar tv
	st <- go pool
	return $ Just (st, StartStage)
  where
	go pool = case spareVals pool of
		[] -> retry
		(v:vs) -> do
			let pool' = pool { spareVals = vs }
			putTMVar tv =<< waitIdleWorkerSlot StartStage pool'
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
