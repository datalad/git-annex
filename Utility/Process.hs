{- System.Process enhancements, including additional ways of running
 - processes, and logging.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP, Rank2Types, LambdaCase #-}
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
	hGetLineUntilExitOrEOF,
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

{- | Like hGetLine, reads a line from the Handle. Returns Nothing if end of
 - file is reached, or the handle is closed, or if the process has exited
 - and there is nothing more buffered to read from the handle.
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
 -
 - Note on newline mode: This ignores whatever newline mode is configured
 - for the handle, because there is no way to query that. On Windows,
 - it will remove any \r coming before the \n. On other platforms,
 - it does not treat \r specially.
 -}
hGetLineUntilExitOrEOF :: ProcessHandle -> Handle -> IO (Maybe String)
hGetLineUntilExitOrEOF ph h = go []
  where
	go buf = do
		ready <- waitforinputorerror smalldelay
		if ready
			then getloop buf go
			else getProcessExitCode ph >>= \case
				-- Process still running, wait longer.
				Nothing -> go buf
				-- Process is done. It's possible
				-- that it output something and exited
				-- since the prior hWaitForInput,
				-- so check one more time for any buffered
				-- output.
				Just _ -> finalcheck buf

	finalcheck buf = do
		ready <- waitforinputorerror 0
		if ready
			then getloop buf finalcheck
			-- No remaining buffered input, though the handle
			-- may not be EOF if something else is keeping it
			-- open. Treated the same as EOF.
			else eofwithnolineend buf

	-- On exception, proceed as if there was input;
	-- EOF and any encoding issues are dealt with
	-- when reading from the handle.
	waitforinputorerror t = hWaitForInput h t
		`catchNonAsync` const (pure True)

	getchar = 
		catcherr EOF $
			-- If the handle is closed, reading from it is
			-- an IllegalOperation.
			catcherr IllegalOperation $
				Just <$> hGetChar h
	  where
		catcherr t = catchIOErrorType t (const (pure Nothing))

	getloop buf cont =
		getchar >>= \case
			Just c
				| c == '\n' -> return (Just (gotline buf))
				| otherwise -> cont (c:buf)
			Nothing -> eofwithnolineend buf

#ifndef mingw32_HOST_OS
	gotline buf = reverse buf
#else
	gotline ('\r':buf) = reverse buf
	gotline buf = reverse buf
#endif

	eofwithnolineend buf = return $
		if null buf 
			then Nothing -- no line read
			else Just (reverse buf)

	-- Tenth of a second delay. If the process exits with the FD being
	-- held open, will wait up to twice this long before returning.
	-- This delay could be made smaller. However, that is an unusual
	-- case, and making it too small would cause lots of wakeups while
	-- waiting for output. Bearing in mind that this could be run on
	-- many processes at the same time.
	smalldelay = 100 -- milliseconds
