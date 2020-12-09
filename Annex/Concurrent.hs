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
import Types.WorkerPool
import Types.CatFileHandles
import Annex.CheckAttr
import Annex.CheckIgnore
import Remote.List

import Control.Concurrent
import Control.Concurrent.STM
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
	-- Make sure that some expensive actions have been done before
	-- starting threads. This way the state has them already run,
	-- and each thread won't try to do them.
	_ <- remoteList

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
	forM_ (M.toList $ Annex.cleanup st') $
		uncurry addCleanup
	Annex.Queue.mergeFrom st'
	changeState $ \s -> s { errcounter = errcounter s + errcounter st' }

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
