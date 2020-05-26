{- git-annex command-line actions and concurrency
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
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
	NonConcurrent -> runnonconcurrent
	Concurrent _ -> runconcurrent
	ConcurrentPerCpu -> runconcurrent
  where
	runnonconcurrent = void $ includeCommandAction start
	runconcurrent = Annex.getState Annex.workers >>= \case
		Nothing -> runnonconcurrent
		Just tv -> 
			liftIO (atomically (waitStartWorkerSlot tv)) >>=
				maybe runnonconcurrent (runconcurrent' tv)
	runconcurrent' tv (workerst, workerstage) = do
		aid <- liftIO $ async $ snd <$> Annex.run workerst
			(concurrentjob workerst)
		liftIO $ atomically $ do
			pool <- takeTMVar tv
			let !pool' = addWorkerPool (ActiveWorker aid workerstage) pool
			putTMVar tv pool'
		void $ liftIO $ forkIO $ debugLocks $ do
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
						_ -> beginjob workerst startmsg' perform'
				Nothing -> noop
		_ -> beginjob workerst startmsg perform
	
	beginjob workerst startmsg perform =
		inOwnConsoleRegion (Annex.output workerst) $ do
			enteringInitialStage
			void $ accountCommandAction startmsg $
				performconcurrent startmsg perform

	-- Like performCommandAction' but the worker thread's stage
	-- is changed before starting the cleanup action.
	performconcurrent startmsg perform = do
		showStartMessage startmsg
		perform >>= \case
			Just cleanup -> enteringStage CleanupStage $ do
				r <- cleanup
				showEndMessage startmsg r
				return r
			Nothing -> do
				showEndMessage startmsg False
				return False

{- Waits for all worker threads to finish and merges their AnnexStates
 - back into the current Annex's state.
 -}
finishCommandActions :: Annex ()
finishCommandActions = Annex.getState Annex.workers >>= \case
	Nothing -> noop
	Just tv -> do
		Annex.changeState $ \s -> s { Annex.workers = Nothing }
		sts <- liftIO $ atomically $ do
			pool <- readTMVar tv
			if allIdle pool
				then return (spareVals pool)
				else retry
		mapM_ mergeState sts

{- Waits for all worker threads that have been started so far to finish. -}
waitForAllRunningCommandActions :: Annex ()
waitForAllRunningCommandActions = Annex.getState Annex.workers >>= \case
	Nothing -> noop
	Just tv -> liftIO $ atomically $ do
		pool <- readTMVar tv
		unless (allIdle pool)
			retry

{- Like commandAction, but without the concurrency. -}
includeCommandAction :: CommandStart -> CommandCleanup
includeCommandAction start =
	start >>= \case
		Nothing -> return True
		Just (startmsg, perform) -> do
			showStartMessage startmsg
			accountCommandAction startmsg $
				performCommandAction' startmsg perform

accountCommandAction :: StartMessage -> CommandCleanup -> CommandCleanup
accountCommandAction startmsg cleanup = tryNonAsync cleanup >>= \case
	Right True -> return True
	Right False -> incerr
	Left err -> case fromException err of
		Just exitcode -> liftIO $ exitWith exitcode
		Nothing -> do
			toplevelWarning True (show err)
			showEndMessage startmsg False
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
callCommandAction' start = 
	start >>= \case
		Nothing -> return Nothing
		Just (startmsg, perform) -> do
			showStartMessage startmsg
			Just <$> performCommandAction' startmsg perform

performCommandAction' :: StartMessage -> CommandPerform -> CommandCleanup
performCommandAction' startmsg perform = 
	perform >>= \case
		Nothing -> do
			showEndMessage startmsg False
			return False
		Just cleanup -> do
			r <- cleanup
			showEndMessage startmsg r
			return r

{- Start concurrency when that has been requested.
 - Should be run wrapping the seek stage of a command.
 -
 - Note that a duplicate of the Annex state is made here, and worker
 - threads use that state. While the worker threads are not actually
 - started here, that has the same effect.
 -}
startConcurrency :: UsedStages -> Annex a -> Annex a
startConcurrency usedstages a = do
	fromcmdline <- Annex.getState Annex.concurrency
	fromgitcfg <- annexJobs <$> Annex.getGitConfig
	let usegitcfg = setConcurrency fromgitcfg
	case (fromcmdline, fromgitcfg) of
		(NonConcurrent, NonConcurrent) -> a
		(Concurrent n, _) ->
			goconcurrent n
		(ConcurrentPerCpu, _) ->
			goconcurrentpercpu
		(NonConcurrent, Concurrent n) -> do
			usegitcfg
			goconcurrent n
		(NonConcurrent, ConcurrentPerCpu) -> do
			usegitcfg
			goconcurrentpercpu
  where
	goconcurrent n = do
		raisecapabilitiesto n
		withMessageState $ \s -> case outputType s of
			NormalOutput -> ifM (liftIO concurrentOutputSupported)
				( Regions.displayConsoleRegions $
					goconcurrent' n True
				, goconcurrent' n False
				)
			_ -> goconcurrent' n False
	goconcurrent' n b = bracket_ (setup n b) cleanup a

	goconcurrentpercpu = goconcurrent =<< liftIO getNumProcessors

	setup n b = do
		setconcurrentoutputenabled b
		initworkerpool n

	cleanup = do
		finishCommandActions
		setconcurrentoutputenabled False

	setconcurrentoutputenabled b = Annex.changeState $ \s ->
		s { Annex.output = (Annex.output s) { concurrentOutputEnabled = b } }

	raisecapabilitiesto n = do
		c <- liftIO getNumCapabilities
		when (n > c) $
			liftIO $ setNumCapabilities n
	
	initworkerpool n = do
		tv <- liftIO newEmptyTMVarIO
		Annex.changeState $ \s -> s { Annex.workers = Just tv }
		st <- dupState
		liftIO $ atomically $ putTMVar tv $
			allocateWorkerPool st (max n 1) usedstages

{- Ensures that only one thread processes a key at a time.
 - Other threads will block until it's done.
 -
 - May be called repeatedly by the same thread without blocking. -}
ensureOnlyActionOn :: Key -> Annex a -> Annex a
ensureOnlyActionOn k a = debugLocks $
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
