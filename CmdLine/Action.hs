{- git-annex command-line actions and concurrency
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module CmdLine.Action where

import Annex.Common
import qualified Annex
import Annex.Concurrent
import Types.Command
import Types.Concurrency
import Messages.Concurrent
import Types.Messages
import Types.WorkerPool
import Remote.List

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import GHC.Conc
import qualified Data.Map.Strict as M
import qualified System.Console.Regions as Regions

{- Runs a command, starting with the check stage, and then
 - the seek stage. Finishes by running the continutation, and 
 - then showing a count of any failures. -}
performCommandAction :: Command -> CommandSeek -> Annex () -> Annex ()
performCommandAction Command { cmdcheck = c, cmdname = name } seek cont = do
	mapM_ runCheck c
	Annex.changeState $ \s -> s { Annex.errcounter = 0 }
	seek
	finishCommandActions
	cont
	showerrcount =<< Annex.getState Annex.errcounter
  where
	showerrcount 0 = noop
	showerrcount cnt = giveup $ name ++ ": " ++ show cnt ++ " failed"

commandActions :: [CommandStart] -> Annex ()
commandActions = mapM_ commandAction

{- Runs one of the actions needed to perform a command.
 - Individual actions can fail without stopping the whole command,
 - including by throwing non-async exceptions.
 - 
 - When concurrency is enabled, a thread is forked off to run the action
 - in the background, as soon as a free worker slot is available.
 
 - This should only be run in the seek stage.
 -}
commandAction :: CommandStart -> Annex ()
commandAction start = Annex.getState Annex.concurrency >>= \case
	NonConcurrent -> void $ includeCommandAction start
	Concurrent n -> runconcurrent n
	ConcurrentPerCpu -> runconcurrent =<< liftIO getNumProcessors
  where
	runconcurrent n = do
		tv <- Annex.getState Annex.workers
		workerst <- waitWorkerSlot n (== PerformStage) tv
		aid <- liftIO $ async $ snd <$> Annex.run workerst
			(concurrentjob workerst)
		liftIO $ atomically $ do
			pool <- takeTMVar tv
			let !pool' = addWorkerPool (ActiveWorker aid PerformStage) pool
			putTMVar tv pool'
		void $ liftIO $ forkIO $ do
			-- accountCommandAction will usually catch
			-- exceptions. Just in case, fall back to the
			-- original workerst.
			workerst' <- either (const workerst) id
				<$> waitCatch aid
			atomically $ do
				pool <- takeTMVar tv
				let !pool' = deactivateWorker pool aid workerst'
				putTMVar tv pool'
	
	concurrentjob workerst = start >>= \case
		Nothing -> noop
		Just (startmsg, perform) ->
			concurrentjob' workerst startmsg perform
	
	concurrentjob' workerst startmsg perform = case mkActionItem startmsg of
		OnlyActionOn k _ -> ensureOnlyActionOn k $
			-- If another job performed the same action while we
			-- waited, there may be nothing left to do, so re-run
			-- the start stage to see if it still wants to do
			-- something.
			start >>= \case
				Just (startmsg', perform') ->
					case mkActionItem startmsg' of
						OnlyActionOn k' _ | k' /= k ->
							concurrentjob' workerst startmsg' perform'
						_ -> mkjob workerst startmsg' perform'
				Nothing -> noop
		_ -> mkjob workerst startmsg perform
	
	mkjob workerst startmsg perform = 
		inOwnConsoleRegion (Annex.output workerst) $
			void $ accountCommandAction $
				performconcurrent startmsg perform

	-- Like callCommandAction, but the start stage has already run,
	-- and the worker thread's stage is changed before starting the
	-- cleanup action.
	performconcurrent startmsg perform = do
		showStartMessage startmsg
		perform >>= \case
			Just cleanup -> do
				changeStageTo CleanupStage
				r <- cleanup
				implicitMessage (showEndResult r)
				return r
			Nothing -> do
				implicitMessage (showEndResult False)
				return False

-- | Wait until there's an idle worker in the pool, remove it from the
-- pool, and return its state.
--
-- If the pool is unallocated, it will be allocated to the specified size.
waitWorkerSlot :: Int -> (WorkerStage -> Bool) -> TMVar (WorkerPool Annex.AnnexState) -> Annex (Annex.AnnexState)
waitWorkerSlot n wantstage tv =
	join $ liftIO $ atomically $ waitWorkerSlot' wantstage tv >>= \case
		Nothing -> return $ do
			-- Generate the remote list now, to avoid
			-- each thread generating it, which would
			-- be more expensive and could cause
			-- threads to contend over eg, calls to
			-- setConfig.
			_ <- remoteList
			st <- dupState
			liftIO $ atomically $ do
				let (WorkerPool l) = allocateWorkerPool st (max n 1)
				let (st', pool) = findidle st [] l
				void $ swapTMVar tv pool
				return st'
		Just st -> return $ return st
 where
	findidle st _ [] = (st, WorkerPool [])
	findidle _ c ((IdleWorker st stage):rest) 
		| wantstage stage = (st, WorkerPool (c ++ rest))
	findidle st c (w:rest) = findidle st (w:c) rest

-- | STM action that waits until there's an idle worker in the worker pool.
--
-- If the worker pool is not already allocated, returns Nothing.
waitWorkerSlot' :: (WorkerStage -> Bool) -> TMVar (WorkerPool Annex.AnnexState) -> STM (Maybe (Annex.AnnexState))
waitWorkerSlot' wantstage tv =
	takeTMVar tv >>= \case
		UnallocatedWorkerPool -> do
			putTMVar tv UnallocatedWorkerPool 
			return Nothing
		WorkerPool l -> do
			(st, pool') <- findidle [] l
			putTMVar tv pool'
			return $ Just st
 where
	findidle _ [] = retry
	findidle c ((IdleWorker st stage):rest) 
		| wantstage stage = return (st, WorkerPool (c ++ rest))
	findidle c (w:rest) = findidle (w:c) rest

{- Waits for all worker threads to finish and merges their AnnexStates
 - back into the current Annex's state.
 -}
finishCommandActions :: Annex ()
finishCommandActions = do
	tv <- Annex.getState Annex.workers
	pool <- liftIO $ atomically $
		swapTMVar tv UnallocatedWorkerPool
	case pool of
		UnallocatedWorkerPool -> noop
		WorkerPool l -> forM_ (mapMaybe workerAsync l) $ \aid ->
			liftIO (waitCatch aid) >>= \case
				Left _ -> noop
				Right st -> mergeState st

{- Changes the current thread's stage in the worker pool.
 -
 - The pool needs to continue to contain the same number of worker threads
 - for each stage. So, an idle worker with the desired stage is found in 
 - the pool (waiting if necessary for one to become idle), and the stages
 - of it and the current thread are swapped.
 -}
changeStageTo :: WorkerStage -> Annex ()
changeStageTo newstage = do
	mytid <- liftIO myThreadId
	tv <- Annex.getState Annex.workers
	liftIO $ atomically $ waitWorkerSlot' (== newstage) tv >>= \case
		Just idlest -> do
			pool <- takeTMVar tv
			let pool' = case removeThreadIdWorkerPool mytid pool of
				Just ((myaid, oldstage), p) ->
					addWorkerPool (IdleWorker idlest oldstage) $
						addWorkerPool (ActiveWorker myaid newstage) p
				Nothing -> pool
			putTMVar tv pool'
		-- No worker pool is allocated, not running in concurrent
		-- mode.
		Nothing -> noop

{- Like commandAction, but without the concurrency. -}
includeCommandAction :: CommandStart -> CommandCleanup
includeCommandAction = accountCommandAction . callCommandAction

accountCommandAction :: CommandCleanup -> CommandCleanup
accountCommandAction a = tryNonAsync a >>= \case
	Right True -> return True
	Right False -> incerr
	Left err -> case fromException err of
		Just exitcode -> liftIO $ exitWith exitcode
		Nothing -> do
			toplevelWarning True (show err)
			implicitMessage showEndFail
			incerr
  where
	incerr = do
		Annex.incError
		return False

{- Runs a single command action through the start, perform and cleanup
 - stages, without catching errors and without incrementing error counter.
 - Useful if one command wants to run part of another command. -}
callCommandAction :: CommandStart -> CommandCleanup
callCommandAction = fromMaybe True <$$> callCommandAction' 

{- Like callCommandAction, but returns Nothing when the command did not
 - perform any action. -}
callCommandAction' :: CommandStart -> Annex (Maybe Bool)
callCommandAction' a = callCommandActionQuiet a >>= \case
	Nothing -> return Nothing
	Just r -> implicitMessage (showEndResult r) >> return (Just r)

callCommandActionQuiet :: CommandStart -> Annex (Maybe Bool)
callCommandActionQuiet start =
	start >>= \case
		Nothing -> return Nothing
		Just (startmsg, perform) -> do
			showStartMessage startmsg
			perform >>= \case
				Nothing -> return (Just False)
				Just cleanup -> Just <$> cleanup

{- Do concurrent output when that has been requested. -}
allowConcurrentOutput :: Annex a -> Annex a
allowConcurrentOutput a = do
	fromcmdline <- Annex.getState Annex.concurrency
	fromgitcfg <- annexJobs <$> Annex.getGitConfig
	let usegitcfg = Annex.changeState $ 
		\c -> c { Annex.concurrency = fromgitcfg }
	case (fromcmdline, fromgitcfg) of
		(NonConcurrent, NonConcurrent) -> a
		(Concurrent n, _) -> do
			raisecapabilitiesto n
			goconcurrent
		(ConcurrentPerCpu, _) -> goconcurrent
		(NonConcurrent, Concurrent n) -> do
			usegitcfg
			raisecapabilitiesto n
			goconcurrent
		(NonConcurrent, ConcurrentPerCpu) -> do
			usegitcfg
			goconcurrent
  where
	goconcurrent = do
		withMessageState $ \s -> case outputType s of
			NormalOutput -> ifM (liftIO concurrentOutputSupported)
				( Regions.displayConsoleRegions $
					goconcurrent' True
				, goconcurrent' False
				)
			_ -> goconcurrent' False
	goconcurrent' b = bracket_ (setup b) cleanup a

	setup = setconcurrentoutputenabled

	cleanup = do
		finishCommandActions
		setconcurrentoutputenabled False

	setconcurrentoutputenabled b = Annex.changeState $ \s ->
		s { Annex.output = (Annex.output s) { concurrentOutputEnabled = b } }

	raisecapabilitiesto n = do
		c <- liftIO getNumCapabilities
		when (n > c) $
			liftIO $ setNumCapabilities n

{- Ensures that only one thread processes a key at a time.
 - Other threads will block until it's done.
 -
 - May be called repeatedly by the same thread without blocking. -}
ensureOnlyActionOn :: Key -> Annex a -> Annex a
ensureOnlyActionOn k a = 
	go =<< Annex.getState Annex.concurrency
  where
	go NonConcurrent = a
	go (Concurrent _) = goconcurrent
	go ConcurrentPerCpu = goconcurrent
	goconcurrent = do
		tv <- Annex.getState Annex.activekeys
		bracket (setup tv) id (const a)
	setup tv = liftIO $ do
		mytid <- myThreadId
		atomically $ do
			m <- readTVar tv
			case M.lookup k m of
				Just tid
					| tid /= mytid -> retry
					| otherwise -> return $ return ()
				Nothing -> do
					writeTVar tv $! M.insert k mytid m
					return $ liftIO $ atomically $
						modifyTVar tv $ M.delete k
