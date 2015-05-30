{- System.Process enhancements, including additional ways of running
 - processes, and logging.
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
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
	checkSuccessProcess,
	ignoreFailureProcess,
	createProcessSuccess,
	createProcessChecked,
	createBackgroundProcess,
	processTranscript,
	processTranscript',
	withHandle,
	withIOHandles,
	withOEHandles,
	withQuietOutput,
	feedWithQuietOutput,
	createProcess,
	startInteractiveProcess,
	stdinHandle,
	stdoutHandle,
	stderrHandle,
	ioHandles,
	processHandle,
	devNull,
) where

import qualified System.Process
import qualified System.Process as X hiding (CreateProcess(..), createProcess, runInteractiveProcess, readProcess, readProcessWithExitCode, system, rawSystem, runInteractiveCommand, runProcess)
import System.Process hiding (createProcess, readProcess)
import System.Exit
import System.IO
import System.Log.Logger
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO
#else
import Control.Applicative
#endif
import Data.Maybe
import Prelude

import Utility.Misc
import Utility.Exception

type CreateProcessRunner = forall a. CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a

data StdHandle = StdinHandle | StdoutHandle | StderrHandle
	deriving (Eq)

-- | Normally, when reading from a process, it does not need to be fed any
-- standard input.
readProcess :: FilePath	-> [String] -> IO String
readProcess cmd args = readProcessEnv cmd args Nothing

readProcessEnv :: FilePath -> [String] -> Maybe [(String, String)] -> IO String
readProcessEnv cmd args environ = readProcess' p
  where
	p = (proc cmd args)
		{ std_out = CreatePipe
		, env = environ
		}

readProcess' :: CreateProcess -> IO String
readProcess' p = withHandle StdoutHandle createProcessSuccess p $ \h -> do
	output  <- hGetContentsStrict h
	hClose h
	return output

-- | Runs an action to write to a process on its stdin, 
-- returns its output, and also allows specifying the environment.
writeReadProcessEnv
	:: FilePath
	-> [String]
	-> Maybe [(String, String)]
	-> (Maybe (Handle -> IO ()))
	-> (Maybe (Handle -> IO ()))
	-> IO String
writeReadProcessEnv cmd args environ writestdin adjusthandle = do
	(Just inh, Just outh, _, pid) <- createProcess p

	maybe (return ()) (\a -> a inh) adjusthandle
	maybe (return ()) (\a -> a outh) adjusthandle

	-- fork off a thread to start consuming the output
	output  <- hGetContents outh
	outMVar <- newEmptyMVar
	_ <- forkIO $ E.evaluate (length output) >> putMVar outMVar ()

	-- now write and flush any input
	maybe (return ()) (\a -> a inh >> hFlush inh) writestdin
	hClose inh -- done with stdin

	-- wait on the output
	takeMVar outMVar
	hClose outh

	-- wait on the process
	forceSuccessProcess p pid

	return output

  where
	p = (proc cmd args)
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		, env = environ
		}

-- | Waits for a ProcessHandle, and throws an IOError if the process
-- did not exit successfully.
forceSuccessProcess :: CreateProcess -> ProcessHandle -> IO ()
forceSuccessProcess p pid = do
	code <- waitForProcess pid
	case code of
		ExitSuccess -> return ()
		ExitFailure n -> fail $ showCmd p ++ " exited " ++ show n

-- | Waits for a ProcessHandle and returns True if it exited successfully.
-- Note that using this with createProcessChecked will throw away
-- the Bool, and is only useful to ignore the exit code of a process,
-- while still waiting for it. -}
checkSuccessProcess :: ProcessHandle -> IO Bool
checkSuccessProcess pid = do
	code <- waitForProcess pid
	return $ code == ExitSuccess

ignoreFailureProcess :: ProcessHandle -> IO Bool
ignoreFailureProcess pid = do
	void $ waitForProcess pid
	return True

-- | Runs createProcess, then an action on its handles, and then
-- forceSuccessProcess.
createProcessSuccess :: CreateProcessRunner
createProcessSuccess p a = createProcessChecked (forceSuccessProcess p) p a

-- | Runs createProcess, then an action on its handles, and then
-- a checker action on its exit code, which must wait for the process.
createProcessChecked :: (ProcessHandle -> IO b) -> CreateProcessRunner
createProcessChecked checker p a = do
	t@(_, _, _, pid) <- createProcess p
	r <- tryNonAsync $ a t
	_ <- checker pid
	either E.throw return r

-- | Leaves the process running, suitable for lazy streaming.
-- Note: Zombies will result, and must be waited on.
createBackgroundProcess :: CreateProcessRunner
createBackgroundProcess p a = a =<< createProcess p

-- | Runs a process, optionally feeding it some input, and
-- returns a transcript combining its stdout and stderr, and
-- whether it succeeded or failed.
processTranscript :: String -> [String] -> (Maybe String) -> IO (String, Bool)
processTranscript cmd opts input = processTranscript' cmd opts Nothing input

processTranscript' :: String -> [String] -> Maybe [(String, String)] -> (Maybe String) -> IO (String, Bool)
processTranscript' cmd opts environ input = do
#ifndef mingw32_HOST_OS
{- This implementation interleves stdout and stderr in exactly the order
 - the process writes them. -}
	(readf, writef) <- System.Posix.IO.createPipe
	readh <- System.Posix.IO.fdToHandle readf
	writeh <- System.Posix.IO.fdToHandle writef
	p@(_, _, _, pid) <- createProcess $
		(proc cmd opts)
			{ std_in = if isJust input then CreatePipe else Inherit
			, std_out = UseHandle writeh
			, std_err = UseHandle writeh
			, env = environ
			}
	hClose writeh

	get <- mkreader readh
	writeinput input p
	transcript <- get

	ok <- checkSuccessProcess pid
	return (transcript, ok)
#else
{- This implementation for Windows puts stderr after stdout. -}
	p@(_, _, _, pid) <- createProcess $
		(proc cmd opts)
			{ std_in = if isJust input then CreatePipe else Inherit
			, std_out = CreatePipe
			, std_err = CreatePipe
			, env = environ
			}

	getout <- mkreader (stdoutHandle p)
	geterr <- mkreader (stderrHandle p)
	writeinput input p
	transcript <- (++) <$> getout <*> geterr

	ok <- checkSuccessProcess pid
	return (transcript, ok)
#endif
  where
	mkreader h = do
		s <- hGetContents h
		v <- newEmptyMVar
		void $ forkIO $ do
			void $ E.evaluate (length s)
			putMVar v ()
		return $ do
			takeMVar v
			return s

	writeinput (Just s) p = do
		let inh = stdinHandle p
		unless (null s) $ do
			hPutStr inh s
			hFlush inh
		hClose inh
	writeinput Nothing _ = return ()

-- | Runs a CreateProcessRunner, on a CreateProcess structure, that
-- is adjusted to pipe only from/to a single StdHandle, and passes
-- the resulting Handle to an action.
withHandle
	:: StdHandle
	-> CreateProcessRunner
	-> CreateProcess
	-> (Handle -> IO a)
	-> IO a
withHandle h creator p a = creator p' $ a . select
  where
	base = p
		{ std_in = Inherit
		, std_out = Inherit
		, std_err = Inherit
		}
	(select, p')
		| h == StdinHandle  =
			(stdinHandle, base { std_in = CreatePipe })
		| h == StdoutHandle =
			(stdoutHandle, base { std_out = CreatePipe })
		| h == StderrHandle =
			(stderrHandle, base { std_err = CreatePipe })

-- | Like withHandle, but passes (stdin, stdout) handles to the action.
withIOHandles
	:: CreateProcessRunner
	-> CreateProcess
	-> ((Handle, Handle) -> IO a)
	-> IO a
withIOHandles creator p a = creator p' $ a . ioHandles
  where
	p' = p
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}

-- | Like withHandle, but passes (stdout, stderr) handles to the action.
withOEHandles
	:: CreateProcessRunner
	-> CreateProcess
	-> ((Handle, Handle) -> IO a)
	-> IO a
withOEHandles creator p a = creator p' $ a . oeHandles
  where
	p' = p
		{ std_in = Inherit
		, std_out = CreatePipe
		, std_err = CreatePipe
		}

-- | Forces the CreateProcessRunner to run quietly;
-- both stdout and stderr are discarded.
withQuietOutput
	:: CreateProcessRunner
	-> CreateProcess
	-> IO ()
withQuietOutput creator p = withFile devNull WriteMode $ \nullh -> do
	let p' = p
		{ std_out = UseHandle nullh
		, std_err = UseHandle nullh
		}
	creator p' $ const $ return ()

-- | Stdout and stderr are discarded, while the process is fed stdin
-- from the handle.
feedWithQuietOutput
	:: CreateProcessRunner
	-> CreateProcess
	-> (Handle -> IO a)
	-> IO a
feedWithQuietOutput creator p a = withFile devNull WriteMode $ \nullh -> do
	let p' = p
		{ std_in = CreatePipe
		, std_out = UseHandle nullh
		, std_err = UseHandle nullh
		}
	creator p' $ a . stdinHandle

devNull :: FilePath
#ifndef mingw32_HOST_OS
devNull = "/dev/null"
#else
devNull = "NUL"
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
ioHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> (Handle, Handle)
ioHandles (Just hin, Just hout, _, _) = (hin, hout)
ioHandles _ = error "expected ioHandles"
oeHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> (Handle, Handle)
oeHandles (_, Just hout, Just herr, _) = (hout, herr)
oeHandles _ = error "expected oeHandles"

processHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> ProcessHandle
processHandle (_, _, _, pid) = pid

-- | Debugging trace for a CreateProcess.
debugProcess :: CreateProcess -> IO ()
debugProcess p = do
	debugM "Utility.Process" $ unwords
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

-- | Wrapper around 'System.Process.createProcess' from System.Process,
-- that does debug logging.
createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
	debugProcess p
	System.Process.createProcess p
