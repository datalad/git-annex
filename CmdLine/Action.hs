{- git-annex command-line actions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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

import Control.Concurrent.Async
import Control.Exception (throwIO)
import Data.Either

#ifdef WITH_CONCURRENTOUTPUT
import qualified System.Console.Regions as Regions
#endif

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
	showerrcount cnt = error $ name ++ ": " ++ show cnt ++ " failed"

{- Runs one of the actions needed to perform a command.
 - Individual actions can fail without stopping the whole command,
 - including by throwing IO errors (but other errors terminate the whole
 - command).
 - 
 - When concurrency is enabled, a thread is forked off to run the action
 - in the background, as soon as a free slot is available.
 
 - This should only be run in the seek stage.
 -}
commandAction :: CommandStart -> Annex ()
commandAction a = go =<< Annex.getState Annex.concurrency
  where
	go (Concurrent n) = do
		ws <- Annex.getState Annex.workers
		(st, ws') <- if null ws
			then do
				st <- dupState
				return (st, replicate (n-1) (Left st))
			else do
				l <- liftIO $ drainTo (n-1) ws
				findFreeSlot l
		w <- liftIO $ async
			$ snd <$> Annex.run st (inOwnConsoleRegion (Annex.output st) run)
		Annex.changeState $ \s -> s { Annex.workers = Right w:ws' }
	go NonConcurrent = run
	run = void $ includeCommandAction a

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
	Annex.changeState $ \s -> s { Annex.workers = [] }
	l <- liftIO $ drainTo 0 ws
	forM_ (lefts l) mergeState

{- Wait for Asyncs from the list to finish, replacing them with their
 - final AnnexStates, until the list of remaining Asyncs is not larger
 - than the specified size, then returns the new list.
 -
 - If the action throws an exception, it is propigated, but first
 - all other actions are waited for, to allow for a clean shutdown.
 -}
drainTo
	:: Int
	-> [Either Annex.AnnexState (Async Annex.AnnexState)]
	-> IO [Either Annex.AnnexState (Async Annex.AnnexState)]
drainTo sz l
	| null as || sz >= length as = return l
	| otherwise = do
		(done, ret) <- waitAnyCatch as
		let as' = filter (/= done) as
		case ret of
			Left e -> do
				void $ drainTo 0 (map Left sts ++ map Right as')
				throwIO e
			Right st -> do
				drainTo sz $ map Left (st:sts) ++ map Right as'
  where
	(sts, as) = partitionEithers l

findFreeSlot :: [Either Annex.AnnexState (Async Annex.AnnexState)] -> Annex (Annex.AnnexState, [Either Annex.AnnexState (Async Annex.AnnexState)])
findFreeSlot = go []
  where
	go c [] = do
		st <- dupState
		return (st, c)
	go c (Left st:rest) = return (st, c ++ rest)
	go c (v:rest) = go (v:c) rest

{- Like commandAction, but without the concurrency. -}
includeCommandAction :: CommandStart -> CommandCleanup
includeCommandAction a = account =<< tryIO (callCommandAction a)
  where
	account (Right True) = return True
	account (Right False) = incerr
	account (Left err) = do
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
callCommandAction' = start
  where
	start   = stage $ maybe skip perform
	perform = stage $ maybe failure cleanup
	cleanup = stage $ status
	stage = (=<<)
	skip = return Nothing
	failure = implicitMessage showEndFail >> return (Just False)
	status r = implicitMessage (showEndResult r) >> return (Just r)

{- Do concurrent output when that has been requested. -}
allowConcurrentOutput :: Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
allowConcurrentOutput a = go =<< Annex.getState Annex.concurrency
  where
	go NonConcurrent = a
	go (Concurrent _) = ifM (liftIO concurrentOutputSupported)
		( Regions.displayConsoleRegions $
			goconcurrent True
		, goconcurrent False
		)
	goconcurrent b = bracket_ (setup b) cleanup a
	setup = setconcurrentenabled
	cleanup = do
		finishCommandActions
		setconcurrentenabled False
	setconcurrentenabled b = Annex.changeState $ \s ->
		s { Annex.output = (Annex.output s) { concurrentOutputEnabled = b } }
#else
allowConcurrentOutput = id
#endif
