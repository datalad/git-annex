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
import Data.Either
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
		ws <- liftIO . drainTo (n-1) =<< Annex.getState Annex.workers
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
			WorkerPool l -> findFreeSlot l
		w <- liftIO $ async $ snd <$> Annex.run st
			(inOwnConsoleRegion (Annex.output st) run)
		Annex.changeState $ \s -> s
			{ Annex.workers = addWorkerPool ws' (Right w) }

commandActions :: [CommandStart] -> Annex ()
commandActions = mapM_ commandAction

{- Waits for any forked off command actions to finish.
 -
 - Merge together the cleanup actions of all the AnnexStates used by
 - threads, into the current Annex's state, so they'll run at shutdown.
 -
 - Also merge together the errcounters of the AnnexStates.
 -}
finishCommandActions :: Annex ()
finishCommandActions = do
	ws <- Annex.getState Annex.workers
	Annex.changeState $ \s -> s { Annex.workers = UnallocatedWorkerPool }
	ws' <- liftIO $ drainTo 0 ws
	forM_ (idleWorkers ws') mergeState

{- Wait for jobs from the WorkerPool to complete, until
 - the number of running jobs is not larger than the specified number.
 -
 - If a job throws an exception, it is propigated, but first
 - all other jobs are waited for, to allow for a clean shutdown.
 -}
drainTo :: Int -> WorkerPool t -> IO (WorkerPool t)
drainTo _ UnallocatedWorkerPool = pure UnallocatedWorkerPool
drainTo sz (WorkerPool l)
	| null as || sz >= length as = pure (WorkerPool l)
	| otherwise = do
		(done, ret) <- waitAnyCatch as
		let as' = filter (/= done) as
		case ret of
			Left e -> do
				void $ drainTo 0 $ WorkerPool $
					map Left sts ++ map Right as'
				throwIO e
			Right st -> do
				drainTo sz $ WorkerPool $
					map Left (st:sts) ++ map Right as'
  where
	(sts, as) = partitionEithers l

findFreeSlot :: [Worker Annex.AnnexState] -> Annex (Annex.AnnexState, WorkerPool Annex.AnnexState)
findFreeSlot = go []
  where
	go c [] = do
		st <- dupState
		return (st, WorkerPool c)
	go c (Left st:rest) = return (st, WorkerPool (c ++ rest))
	go c (v:rest) = go (v:c) rest

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
	perform = stage $ maybe failure cleanup
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
