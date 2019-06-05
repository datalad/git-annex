{- git-annex command-line actions
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

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
import Control.Exception (throwIO)
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

{- Runs one of the actions needed to perform a command.
 - Individual actions can fail without stopping the whole command,
 - including by throwing non-async exceptions.
 - 
 - When concurrency is enabled, a thread is forked off to run the action
 - in the background, as soon as a free slot is available.
 
 - This should only be run in the seek stage.
 -}
commandAction :: CommandStart -> Annex ()
commandAction a = Annex.getState Annex.concurrency >>= \case
	NonConcurrent -> run
	Concurrent n -> runconcurrent n
	ConcurrentPerCpu -> runconcurrent =<< liftIO getNumProcessors
  where
	run = void $ includeCommandAction a

	runconcurrent n = do
		tv <- Annex.getState Annex.workers
		ws <- liftIO $ drainTo (n-1) (== PerformStage) 
			=<< atomically (takeTMVar tv)
		(st, ws') <- case ws of
			UnallocatedWorkerPool -> do
				-- Generate the remote list now, to avoid
				-- each thread generating it, which would
				-- be more expensive and could cause
				-- threads to contend over eg, calls to
				-- setConfig.
				_ <- remoteList
				st <- dupState
				return (st, allocateWorkerPool st (n-1))
			WorkerPool _ -> findFreeSlot (== PerformStage) ws
		w <- liftIO $ async $ snd <$> Annex.run st
			(inOwnConsoleRegion (Annex.output st) run)
		liftIO $ atomically $ putTMVar tv $
			addWorkerPool (ActiveWorker w PerformStage) ws'

commandActions :: [CommandStart] -> Annex ()
commandActions = mapM_ commandAction

{- Waits for any worker threads to finish.
 -
 - Merge the AnnexStates used by the threads back into the current Annex's
 - state.
 -}
finishCommandActions :: Annex ()
finishCommandActions = do
	tv <- Annex.getState Annex.workers
	let get = liftIO $ atomically $ takeTMVar tv
	let put = liftIO . atomically . putTMVar tv
	bracketOnError get put $ \ws -> do
		ws' <- liftIO $ drainTo 0 (const True) ws
		forM_ (idleWorkers ws') mergeState
		put UnallocatedWorkerPool

{- Wait for jobs from the WorkerPool to complete, until
 - the number of running jobs of the desired stage
 - is not larger than the specified number.
 -
 - If a job throws an exception, it is propigated, but first
 - all other jobs are waited for, to allow for a clean shutdown.
 -}
drainTo :: Int -> (WorkerStage -> Bool) -> WorkerPool t -> IO (WorkerPool t)
drainTo _ _ UnallocatedWorkerPool = pure UnallocatedWorkerPool
drainTo sz wantstage (WorkerPool l)
	| null as || sz >= length as = pure (WorkerPool l)
	| otherwise = do
		(done, ret) <- waitAnyCatch (mapMaybe workerAsync as)
		let (ActiveWorker _ donestage:[], as') =
			partition (\w -> workerAsync w == Just done) as
		case ret of
			Left e -> do
				void $ drainTo 0 (const True) $ WorkerPool $
					sts ++ as' ++ otheras
				throwIO e
			Right st -> do
				let w = IdleWorker st donestage
				drainTo sz wantstage $ WorkerPool $
					w : sts ++ as' ++ otheras
  where
	(sts, allas) = partition isidle l
	(as, otheras) = partition (wantstage . workerStage) allas
	isidle (IdleWorker _ _) = True
	isidle (ActiveWorker _ _) = False

findFreeSlot :: (WorkerStage -> Bool) -> WorkerPool Annex.AnnexState -> Annex (Annex.AnnexState, WorkerPool Annex.AnnexState)
findFreeSlot wantstage (WorkerPool l) = go [] l
  where
	go c [] = do
		st <- dupState
		return (st, WorkerPool c)
	go c ((IdleWorker st stage):rest) | wantstage stage = 
		return (st, WorkerPool (c ++ rest))
	go c (v:rest) = go (v:c) rest
findFreeSlot _ UnallocatedWorkerPool = do
	st <- dupState
	return (st, UnallocatedWorkerPool)

{- Changes the current thread's stage in the worker pool.
 -
 - An idle worker with the desired stage is found in the pool
 - (waiting if necessary for one to become idle)
 - and the stages of it and the current thread are swapped.
 -}
changeStageTo :: WorkerStage -> Annex ()
changeStageTo newstage = Annex.getState Annex.concurrency >>= \case
	NonConcurrent -> noop
	Concurrent n -> go n
	ConcurrentPerCpu -> go =<< liftIO getNumProcessors
  where
	go n = do
		tv <- Annex.getState Annex.workers
		let get = liftIO $ atomically $ takeTMVar tv
		let put = liftIO . atomically . putTMVar tv
		bracketOnError get put $ \pool -> do
			pool' <- liftIO $ drainTo (n-1) (== newstage) pool
			(idlest, pool'') <- findFreeSlot (== newstage) pool'
			mytid <- liftIO myThreadId
			case removeThreadIdWorkerPool mytid pool'' of
				Just ((myaid, oldstage), pool''') -> do
					liftIO $ print "switching"
					put $ addWorkerPool (IdleWorker idlest oldstage) $
						addWorkerPool (ActiveWorker myaid newstage) pool'''
				Nothing -> put pool'

{- Like commandAction, but without the concurrency. -}
includeCommandAction :: CommandStart -> CommandCleanup
includeCommandAction a = account =<< tryNonAsync (callCommandAction a)
  where
	account (Right True) = return True
	account (Right False) = incerr
	account (Left err) = case fromException err of
		Just exitcode -> liftIO $ exitWith exitcode
		Nothing -> do
			toplevelWarning True (show err)
			implicitMessage showEndFail
			incerr
	incerr = do
		Annex.incError
		return False

{- Runs a single command action through the start, perform and cleanup
 - stages, without catching errors. Useful if one command wants to run
 - part of another command. -}
callCommandAction :: CommandStart -> CommandCleanup
callCommandAction = fromMaybe True <$$> callCommandAction' 

{- Like callCommandAction, but returns Nothing when the command did not
 - perform any action. -}
callCommandAction' :: CommandStart -> Annex (Maybe Bool)
callCommandAction' a = callCommandActionQuiet a >>= \case
	Nothing -> return Nothing
	Just r -> implicitMessage (showEndResult r) >> return (Just r)

callCommandActionQuiet :: CommandStart -> Annex (Maybe Bool)
callCommandActionQuiet = start
  where
	start   = stage $ maybe skip perform
	perform = stage $ maybe failure $ \a -> do
		changeStageTo CleanupStage
		cleanup a
	cleanup = stage $ status
	stage = (=<<)
	skip = return Nothing
	failure = return (Just False)
	status = return . Just

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
 - Other threads will block until it's done. -}
onlyActionOn :: Key -> CommandStart -> CommandStart
onlyActionOn k a = onlyActionOn' k run
  where
	-- Run whole action, not just start stage, so other threads
	-- block until it's done.
	run = callCommandActionQuiet a >>= \case
		Nothing -> return Nothing
		Just r' -> return $ Just $ return $ Just $ return r'

onlyActionOn' :: Key -> Annex a -> Annex a
onlyActionOn' k a = go =<< Annex.getState Annex.concurrency
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
					| otherwise -> return (return ())
				Nothing -> do
					writeTVar tv $! M.insert k mytid m
					return $ liftIO $ atomically $
						modifyTVar tv $ M.delete k
