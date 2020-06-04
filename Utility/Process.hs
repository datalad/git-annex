{- System.Process enhancements, including additional ways of running
 - processes, and logging.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Process (
	module X,
	CreateProcess(..),
	StdHandle(..),
	readProcess,
	readProcess',
	readProcessEnv,
	writeReadProcessEnv,
	forceSuccessProcess,
	forceSuccessProcess',
	checkSuccessProcess,
	withNullHandle,
	createProcess,
	waitForProcess,
	startInteractiveProcess,
	stdinHandle,
	stdoutHandle,
	stderrHandle,
	processHandle,
	devNull,
) where

import qualified Utility.Process.Shim
import qualified Utility.Process.Shim as X hiding (CreateProcess(..), createProcess, runInteractiveProcess, readProcess, readProcessWithExitCode, system, rawSystem, runInteractiveCommand, runProcess)
import Utility.Process.Shim hiding (createProcess, readProcess, waitForProcess)
import Utility.Misc
import Utility.Exception
import Utility.Monad

import System.Exit
import System.IO
import System.Log.Logger
import Control.Monad.IO.Class
import Control.Concurrent.Async
import qualified Data.ByteString as S

data StdHandle = StdinHandle | StdoutHandle | StderrHandle
	deriving (Eq)

-- | Normally, when reading from a process, it does not need to be fed any
-- standard input.
readProcess :: FilePath	-> [String] -> IO String
readProcess cmd args = readProcess' (proc cmd args)

readProcessEnv :: FilePath -> [String] -> Maybe [(String, String)] -> IO String
readProcessEnv cmd args environ = 
	readProcess' $ (proc cmd args) { env = environ }

readProcess' :: CreateProcess -> IO String
readProcess' p = withCreateProcess p' go
  where
	p' = p { std_out = CreatePipe }
	go _ (Just h) _ pid = do
		output  <- hGetContentsStrict h
		hClose h
		forceSuccessProcess p' pid
		return output
	go _ _ _ _ = error "internal"

-- | Runs an action to write to a process on its stdin, 
-- returns its output, and also allows specifying the environment.
writeReadProcessEnv
	:: FilePath
	-> [String]
	-> Maybe [(String, String)]
	-> (Maybe (Handle -> IO ()))
	-> IO S.ByteString
writeReadProcessEnv cmd args environ writestdin = withCreateProcess p go
  where
	p = (proc cmd args)
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		, env = environ
		}
	
	go (Just inh) (Just outh) _ pid = do
		let reader = hClose outh `after` S.hGetContents outh
		let writer = do
			maybe (return ()) (\a -> a inh >> hFlush inh) writestdin
			hClose inh
		(output, ()) <- concurrently reader writer

		forceSuccessProcess p pid

		return output
	go _ _ _ _ = error "internal"

-- | Waits for a ProcessHandle, and throws an IOError if the process
-- did not exit successfully.
forceSuccessProcess :: CreateProcess -> ProcessHandle -> IO ()
forceSuccessProcess p pid = waitForProcess pid >>= forceSuccessProcess' p

forceSuccessProcess' :: CreateProcess -> ExitCode -> IO ()
forceSuccessProcess' _ ExitSuccess = return ()
forceSuccessProcess' p (ExitFailure n) = fail $
	showCmd p ++ " exited " ++ show n

-- | Waits for a ProcessHandle and returns True if it exited successfully.
checkSuccessProcess :: ProcessHandle -> IO Bool
checkSuccessProcess pid = do
	code <- waitForProcess pid
	return $ code == ExitSuccess

withNullHandle :: (MonadIO m, MonadMask m) => (Handle -> m a) -> m a
withNullHandle = bracket
	(liftIO $ openFile devNull WriteMode)
	(liftIO . hClose)

devNull :: FilePath
#ifndef mingw32_HOST_OS
devNull = "/dev/null"
#else
-- Use device namespace to prevent GHC from rewriting path
devNull = "\\\\.\\NUL"
#endif

-- | Extract a desired handle from createProcess's tuple.
-- These partial functions are safe as long as createProcess is run
-- with appropriate parameters to set up the desired handle.
-- Get it wrong and the runtime crash will always happen, so should be
-- easily noticed.
type HandleExtractor = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Handle

stdinHandle :: HandleExtractor
stdinHandle (Just h, _, _, _) = h
stdinHandle _ = error "expected stdinHandle"
stdoutHandle :: HandleExtractor
stdoutHandle (_, Just h, _, _) = h
stdoutHandle _ = error "expected stdoutHandle"
stderrHandle :: HandleExtractor
stderrHandle (_, _, Just h, _) = h
stderrHandle _ = error "expected stderrHandle"

processHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> ProcessHandle
processHandle (_, _, _, pid) = pid

-- | Shows the command that a CreateProcess will run.
showCmd :: CreateProcess -> String
showCmd = go . cmdspec
  where
	go (ShellCommand s) = s
	go (RawCommand c ps) = c ++ " " ++ show ps

-- | Starts an interactive process. Unlike runInteractiveProcess in
-- System.Process, stderr is inherited.
startInteractiveProcess
	:: FilePath
	-> [String]
	-> Maybe [(String, String)]
	-> IO (ProcessHandle, Handle, Handle)
startInteractiveProcess cmd args environ = do
	let p = (proc cmd args)
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		, env = environ
		}
	(Just from, Just to, _, pid) <- createProcess p
	return (pid, to, from)

-- | Wrapper around 'System.Process.createProcess' that does debug logging.
createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
	debugProcess p
	Utility.Process.Shim.createProcess p

-- | Debugging trace for a CreateProcess.
debugProcess :: CreateProcess -> IO ()
debugProcess p = debugM "Utility.Process" $ unwords
	[ action ++ ":"
	, showCmd p
	]
  where
	action
		| piped (std_in p) && piped (std_out p) = "chat"
		| piped (std_in p)                      = "feed"
		| piped (std_out p)                     = "read"
		| otherwise                             = "call"
	piped Inherit = False
	piped _ = True

-- | Wrapper around 'System.Process.waitForProcess' that does debug logging.
waitForProcess ::  ProcessHandle -> IO ExitCode
waitForProcess h = do
	r <- Utility.Process.Shim.waitForProcess h
	debugM "Utility.Process" ("process done " ++ show r)
	return r
