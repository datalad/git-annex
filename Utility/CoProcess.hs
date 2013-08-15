{- Interface for running a shell command as a coprocess,
 - sending it queries and getting back results.
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.CoProcess (
	CoProcessHandle,
	start,
	stop,
	query,
	rawMode
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
	{ coProcessRestartable :: Bool
	, coProcessCmd :: FilePath
	, coProcessParams :: [String]
	, coProcessEnv :: Maybe [(String, String)]
	}

start :: Bool -> FilePath -> [String] -> Maybe [(String, String)] -> IO CoProcessHandle
start restartable cmd params env = do
	s <- start' $ CoProcessSpec restartable cmd params env
	newMVar s

start' :: CoProcessSpec -> IO CoProcessState
start' s = do
	(pid, from, to) <- startInteractiveProcess (coProcessCmd s) (coProcessParams s) (coProcessEnv s)
	return $ CoProcessState pid to from s

stop :: CoProcessHandle -> IO ()
stop ch = do
	s <- readMVar ch
	hClose $ coProcessTo s
	hClose $ coProcessFrom s
	let p = proc (coProcessCmd $ coProcessSpec s) (coProcessParams $ coProcessSpec s)
	forceSuccessProcess p (coProcessPid s)

{- To handle a restartable process, any IO exception thrown by the send and
 - receive actions are assumed to mean communication with the process
 - failed, and the failed action is re-run with a new process. -}
query :: CoProcessHandle -> (Handle -> IO a) -> (Handle -> IO b) -> IO b
query ch send receive = do
	s <- readMVar ch
	restartable s (send $ coProcessTo s) $ const $
		restartable s (hFlush $ coProcessTo s) $ const $
			restartable s (receive $ coProcessFrom s) $
				return
  where
  	restartable s a cont
		| coProcessRestartable (coProcessSpec s) =
			maybe restart cont =<< catchMaybeIO a
		| otherwise = cont =<< a
	restart = do
		s <- takeMVar ch
		void $ catchMaybeIO $ do
			hClose $ coProcessTo s
			hClose $ coProcessFrom s
		void $ waitForProcess $ coProcessPid s
		s' <- start' (coProcessSpec s)
		putMVar ch s'
		query ch send receive

rawMode :: CoProcessHandle -> IO CoProcessHandle
rawMode ch = do
	s <- readMVar ch
	raw $ coProcessFrom s
	raw $ coProcessTo s
	return ch
  where
  	raw h = do
		fileEncoding h
#ifdef mingw32_HOST_OS
		hSetNewlineMode h noNewlineTranslation
#endif
