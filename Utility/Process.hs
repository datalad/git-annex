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
	withCreateProcess,
	waitForProcess,
	cleanupProcess,
	cancelOnExit,
	startInteractiveProcess,
	stdinHandle,
	stdoutHandle,
	stderrHandle,
	processHandle,
	devNull,
) where

import qualified Utility.Process.Shim
import Utility.Process.Shim as X (CreateProcess(..), ProcessHandle, StdStream(..), CmdSpec(..), proc, getPid, getProcessExitCode, shell, terminateProcess)
import Utility.Misc
import Utility.Exception
import Utility.Monad

import System.Exit
import System.IO
import System.Log.Logger
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as S
import GHC.IO.Handle (hWaitForInput)

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
	r@(_, _, _, h) <- Utility.Process.Shim.createProcess p
	debugProcess p h
	return r

-- | Wrapper around 'System.Process.withCreateProcess' that does debug logging.
withCreateProcess :: CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withCreateProcess p action = bracket (createProcess p) cleanupProcess
	(\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- | Debugging trace for a CreateProcess.
debugProcess :: CreateProcess -> ProcessHandle -> IO ()
debugProcess p h = do
	pid <- getPid h
	debugM "Utility.Process" $ unwords
		[ describePid pid
		, action ++ ":"
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

describePid :: Maybe Utility.Process.Shim.Pid -> String
describePid Nothing = "process"
describePid (Just p) = "process [" ++  show p ++ "]"

-- | Wrapper around 'System.Process.waitForProcess' that does debug logging.
waitForProcess ::  ProcessHandle -> IO ExitCode
waitForProcess h = do
	-- Have to get pid before waiting, which closes the ProcessHandle.
	pid <- getPid h
	r <- Utility.Process.Shim.waitForProcess h
	debugM "Utility.Process" (describePid pid ++ " done " ++ show r)
	return r

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO () 
#if MIN_VERSION_process(1,6,4)
cleanupProcess = Utility.Process.Shim.cleanupProcess
#else
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, pid) = do
	-- Unlike the real cleanupProcess, this does not wait
	-- for the process to finish in the background, so if
	-- the process ignores SIGTERM, this can block until the process
	-- gets around the exiting.
	terminateProcess pid
	let void _ = return ()
	maybe (return ()) (void . tryNonAsync . hClose) mb_stdin
	maybe (return ()) hClose mb_stdout
	maybe (return ()) hClose mb_stderr
	void $ waitForProcess pid
#endif

{- | Like hGetLine, reads a line from the Handle. If the Handle is already
 - closed, returns Nothing. If the process exits without writing a line,
 - also returns Nothing.
 -
 - This is useful to protect against situations where the process might
 - have transferred the handle being read to another process, and so
 - the handle could remain open after the process has exited. That is a rare
 - situation, but can happen. Consider a the process that started up a
 - daemon, and the daemon inherited stderr from it, rather than the more
 - usual behavior of closing the file descriptor. Reading from stderr
 - would block past the exit of the process.
 -
 - In that situation, this will detect when the process has exited,
 - and avoid blocking forever. But will still return anything the process
 - buffered to the handle before exiting.
 -}
hGetLineUntilExitOrEOF :: ProcessHandle -> Handle -> IO (Maybe String)
hGetLineUntilExitOrEOF ph h = either Just id <$> (reader `race` waiter)
  where
	reader = hGetLine isEOF h >>= \case
		True -> return Nothing
		False -> Just <$> hGetLine h

	waiter = do
		smalldelay
		_ <- waitForProcess ph
		waiter'
		-- Reached the end of the processes output.
		hClose h

	waiter' = isanythingbuffered >>= \case
		True -> do
			-- Waiting for the reader to consume
			-- buffered output after the process has
			-- exited.
			threadDelay 10000 -- 1/100th second
			waiter'
		False ->  return ()

	isanythingbuffered = 
		-- waitForInput is documented to throw an encoding
		-- error in some cases, if the Handle has buffered on it
		-- something that cannot be decoded. If it does,
		-- that does imply there's still something buffered though.
		catchNonAsync
			(const (return True))
			-- waitForInput can throw an EOF error
			(catchIOErrorType EOF
				(const (return False))
				(hWaitForInput h 0))


	-- A small delay avoids starting the work of waitForProcess
	-- unncessarily in the common case where hGetLine gets a buffered
	-- line immediately.
	smalldelay = threadDelay 10000 -- 1/100th second
