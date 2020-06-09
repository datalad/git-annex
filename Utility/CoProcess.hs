{- Interface for running a shell command as a coprocess,
 - sending it queries and getting back results.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.CoProcess (
	CoProcessHandle,
	CoProcessState(..),
	start,
	stop,
	query,
) where

import Common

import Control.Concurrent.MVar

type CoProcessHandle = MVar CoProcessState

data CoProcessState = CoProcessState
	{ coProcessPid :: ProcessHandle
	, coProcessTo :: Handle
	, coProcessFrom :: Handle
	, coProcessSpec :: CoProcessSpec
	}

data CoProcessSpec = CoProcessSpec
	{ coProcessNumRestarts :: Int
	, coProcessCmd :: FilePath
	, coProcessParams :: [String]
	, coProcessEnv :: Maybe [(String, String)]
	}

start :: Int -> FilePath -> [String] -> Maybe [(String, String)] -> IO CoProcessHandle
start numrestarts cmd params environ = do
	s <- start' $ CoProcessSpec numrestarts cmd params environ
	newMVar s

start' :: CoProcessSpec -> IO CoProcessState
start' s = do
	(pid, from, to) <- startInteractiveProcess (coProcessCmd s) (coProcessParams s) (coProcessEnv s)
	rawMode from
	rawMode to
	return $ CoProcessState pid to from s
  where
#ifdef mingw32_HOST_OS
	rawMode h = hSetNewlineMode h noNewlineTranslation
#else
	rawMode _ = return ()
#endif

stop :: CoProcessHandle -> IO ()
stop ch = do
	s <- readMVar ch
	hClose $ coProcessTo s
	hClose $ coProcessFrom s
	let p = proc (coProcessCmd $ coProcessSpec s) (coProcessParams $ coProcessSpec s)
	forceSuccessProcess p (coProcessPid s)

{- Note that concurrent queries are not safe to perform; caller should
 - serialize calls to query.
 -
 - To handle a restartable process, any IO exception thrown by the send and
 - receive actions are assumed to mean communication with the process
 - failed, and the query is re-run with a new process.
 -
 - If an async exception is received during a query, the state of
 - communication with the process is unknown, so it is killed, and a new
 - one started so the CoProcessHandle can continue to be used by other
 - threads.
 -}
query :: CoProcessHandle -> (Handle -> IO a) -> (Handle -> IO b) -> IO b
query ch send receive = uninterruptibleMask $ \unmask ->
	unmask (readMVar ch >>= restartable)
		`catchAsync` forcerestart
  where
	go s = do
		void $ send $ coProcessTo s
		hFlush $ coProcessTo s
		receive $ coProcessFrom s
	
	restartable s
		| coProcessNumRestarts (coProcessSpec s) > 0 =
			catchMaybeIO (go s)
				>>= maybe (restart s increstarts restartable) return
		| otherwise = go s
	
	increstarts s = s { coProcessNumRestarts = coProcessNumRestarts s - 1 }

	restart s f cont = do
		void $ tryNonAsync $ do
			hClose $ coProcessTo s
			hClose $ coProcessFrom s
		void $ waitForProcess $ coProcessPid s
		s' <- withMVarMasked ch $ \_ -> start' (f (coProcessSpec s))
		cont s'

	forcerestart ex = do
		s <- readMVar ch
		terminateProcess (coProcessPid s)
		restart s id $ \s' -> void $ swapMVar ch s'
		either throwM throwM ex
