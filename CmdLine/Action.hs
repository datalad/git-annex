{- git-annex command-line actions and concurrency
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module CmdLine.Action where

import Annex.Common
import qualified Annex
import Annex.Concurrent
import Annex.WorkerPool
import Types.Command
import Types.Concurrency
import Annex.Content
import Messages.Concurrent
import Types.Messages
import Types.WorkerPool
import Types.ActionItem
import Remote.List

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import GHC.Conc
import qualified Data.Map.Strict as M
import qualified System.Console.Regions as Regions

{- Runs a command, starting with the check stage, and then
 - the seek stage. Finishes by running the continuation.
 -
 - Can exit when there was a problem or when a time or size limit was
 - reached. Also shows a count of any failures when that is enabled.
 -}
performCommandAction :: Bool -> Command -> CommandSeek -> Annex () -> Annex ()
performCommandAction canexit (Command { cmdcheck = c, cmdname = name }) seek cont = do
	mapM_ runCheck c
	Annex.changeState $ \s -> s { Annex.errcounter = 0 }
	seek
	finishCommandActions
	cont
	st <- Annex.getState id
	when canexit $ liftIO $ case (Annex.errcounter st, Annex.reachedlimit st) of
		(0, False) -> noop
		(errcnt, False) -> do
			showerrcount errcnt
			exitWith $ ExitFailure 1
		(0, True) -> exitreachedlimit
		(errcnt, True) -> do
			showerrcount errcnt
			exitreachedlimit
  where
	showerrcount cnt = hPutStrLn stderr $
		name ++ ": " ++ show cnt ++ " failed"
	exitreachedlimit = exitWith $ ExitFailure 101

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
commandAction start = do
	st <- Annex.getState id
	case getConcurrency' (Annex.concurrency st) of
		NonConcurrent -> runnonconcurrent (Annex.sizelimit st)
		Concurrent _ -> runconcurrent (Annex.sizelimit st) (Annex.workers st)
		ConcurrentPerCpu -> runconcurrent (Annex.sizelimit st) (Annex.workers st)
  where
	runnonconcurrent sizelimit = start >>= \case
		Nothing -> noop
		Just (startmsg, perform) -> 
			checkSizeLimit sizelimit startmsg $ do
				showStartMessage startmsg
				void $ accountCommandAction startmsg $
					performCommandAction' startmsg perform

	runconcurrent sizelimit Nothing = runnonconcurrent sizelimit
	runconcurrent sizelimit (Just tv) = 
		liftIO (atomically (waitStartWorkerSlot tv)) >>= maybe
			(runnonconcurrent sizelimit)
			(runconcurrent' sizelimit tv)
	runconcurrent' sizelimit tv (workerstrd, workerstage) = do
		aid <- liftIO $ async $ snd 
			<$> Annex.run workerstrd
				(concurrentjob sizelimit (fst workerstrd))
		liftIO $ atomically $ do
			pool <- takeTMVar tv
			let !pool' = addWorkerPool (ActiveWorker aid workerstage) pool
			putTMVar tv pool'
		void $ liftIO $ forkIO $ debugLocks $ do
			-- accountCommandAction will usually catch
			-- exceptions. Just in case, fall back to the
			-- original workerstrd.
			workerstrd' <- either (const workerstrd) id
				<$> waitCatch aid
			atomically $ do
				pool <- takeTMVar tv
				let !pool' = deactivateWorker pool aid workerstrd'
				putTMVar tv pool'
	
	concurrentjob sizelimit workerst = start >>= \case
		Nothing -> noop
		Just (startmsg, perform) ->
			checkSizeLimit sizelimit startmsg $
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
	
{- Waits for all worker thrneads to finish and merges their AnnexStates
 - back into the current Annex's state.
 -}
finishCommandActions :: Annex ()
finishCommandActions = Annex.getState Annex.workers >>= \case
	Nothing -> noop
	Just tv -> do
		Annex.changeState $ \s -> s { Annex.workers = Nothing }
		vs <- liftIO $ atomically $ do
			pool <- readTMVar tv
			if allIdle pool
				then return (spareVals pool)
				else retry
		mapM_ (mergeState . fst) vs

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
			toplevelWarning True (UnquotedString (show err))
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
callCommandAction start = start >>= \case
	Just (startmsg, perform) -> do
		showStartMessage startmsg
		performCommandAction' startmsg perform
	Nothing -> return True

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
	fromcmdline <- getConcurrency
	fromgitcfg <- annexJobs <$> Annex.getGitConfig
	let usegitcfg = setConcurrency (ConcurrencyGitConfig fromgitcfg)
	case (fromcmdline, fromgitcfg) of
		(NonConcurrent, NonConcurrent) -> a
		(Concurrent n, _) 
			| n > 1 -> goconcurrent n
			| otherwise -> do
				setConcurrency' NonConcurrent ConcurrencyCmdLine
				a
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
		prepDupState
		st <- dupState
		rd <- Annex.getRead id
		liftIO $ atomically $ putTMVar tv $
			allocateWorkerPool (st, rd) (max n 1) usedstages

-- Make sure that some expensive actions have been done before
-- starting threads. This way the state has them already run,
-- and each thread won't try to do them.
prepDupState :: Annex ()
prepDupState = do
	_ <- remoteList
	return ()

{- Ensures that only one thread processes a key at a time.
 - Other threads will block until it's done.
 -
 - May be called repeatedly by the same thread without blocking. -}
ensureOnlyActionOn :: Key -> Annex a -> Annex a
ensureOnlyActionOn k a = debugLocks $
	go =<< getConcurrency
  where
	go NonConcurrent = a
	go (Concurrent _) = goconcurrent
	go ConcurrentPerCpu = goconcurrent
	goconcurrent = do
		tv <- Annex.getRead Annex.activekeys
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

checkSizeLimit :: Maybe (TVar Integer) -> StartMessage -> Annex () -> Annex ()
checkSizeLimit Nothing _ a = a
checkSizeLimit (Just sizelimitvar) startmsg a =
	case actionItemKey (mkActionItem startmsg) of
		Just k -> case fromKey keySize k of
			Just sz -> go sz
			Nothing -> do
				fsz <- catchMaybeIO $ withObjectLoc k $
					liftIO . getFileSize
				maybe reachedlimit go fsz
		Nothing -> a
  where
	go sz = do
		fits <- liftIO $ atomically $ do
			n <- readTVar sizelimitvar
			let !n' = n - sz
			if n' >= 0
				then do
					writeTVar sizelimitvar n'
					return True
				else return False
		if fits 
			then a
			else reachedlimit
	
	reachedlimit = Annex.changeState $ \s -> s { Annex.reachedlimit = True }
