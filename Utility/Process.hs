{- System.Process enhancements, including additional ways of running
 - processes, and logging.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE Rank2Types #-}

module Utility.Process (
	module X,
	CreateProcess,
	StdHandle(..),
	readProcessEnv,
	forceSuccessProcess,
	checkSuccessProcess,
	createProcessSuccess,
	createProcessChecked,
	createBackgroundProcess,
	withHandle,
	withBothHandles,
	createProcess,
	runInteractiveProcess,
	readProcess
) where

import qualified System.Process
import System.Process as X hiding (CreateProcess(..), createProcess, runInteractiveProcess, readProcess, readProcessWithExitCode, system, rawSystem, runInteractiveCommand, runProcess)
import System.Process hiding (createProcess, runInteractiveProcess, readProcess, readProcessWithExitCode)
import System.Exit
import System.IO
import System.Log.Logger

import Utility.Misc

type CreateProcessRunner = forall a. CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a

data StdHandle = StdinHandle | StdoutHandle | StderrHandle
	deriving (Eq)

{- Like readProcess, but allows specifying the environment, and does
 - not mess with stdin. -}
readProcessEnv :: FilePath -> [String] -> Maybe [(String, String)] -> IO String
readProcessEnv cmd args environ =
	withHandle StdoutHandle createProcessSuccess p $ \h -> do
		output  <- hGetContentsStrict h
		hClose h
		return output
	where
		p = (proc cmd args)
			{ std_out = CreatePipe
			, env = environ
			}

{- Waits for a ProcessHandle, and throws an exception if the process
 - did not exit successfully. -}
forceSuccessProcess :: CreateProcess -> ProcessHandle -> IO ()
forceSuccessProcess p pid = do
	code <- waitForProcess pid
	case code of
		ExitSuccess -> return ()
		ExitFailure n -> error $ showCmd p ++ " exited " ++ show n

{- Waits for a ProcessHandle and returns True if it exited successfully. -}
checkSuccessProcess :: ProcessHandle -> IO Bool
checkSuccessProcess pid = do
	code <- waitForProcess pid
	return $ code == ExitSuccess

{- Runs createProcess, then an action on its handles, and then
 - forceSuccessProcess. -}
createProcessSuccess :: CreateProcessRunner
createProcessSuccess p a = createProcessChecked (forceSuccessProcess p) p a

{- Runs createProcess, then an action on its handles, and then
 - an action on its exit code. -}
createProcessChecked :: (ProcessHandle -> IO b) -> CreateProcessRunner
createProcessChecked checker p a = do
	t@(_, _, _, pid) <- createProcess p
	r <- a t
	_ <- checker pid
	return r

{- Leaves the process running, suitable for lazy streaming.
 - Note: Zombies will result, and must be waited on. -}
createBackgroundProcess :: CreateProcessRunner
createBackgroundProcess p a = a =<< createProcess p

{- Runs a CreateProcessRunner, on a CreateProcess structure, that
 - is adjusted to pipe only from/to a single StdHandle, and passes
 - the resulting Handle to an action. -}
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

{- Like withHandle, but passes (stdin, stdout) handles to the action. -}
withBothHandles
	:: CreateProcessRunner
	-> CreateProcess
	-> ((Handle, Handle) -> IO a)
	-> IO a
withBothHandles creator p a = creator p' $ a . bothHandles
	where
		p' = p
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = Inherit
			}

{- Extract a desired handle from createProcess's tuple.
 - These partial functions are safe as long as createProcess is run
 - with appropriate parameters to set up the desired handle.
 - Get it wrong and the runtime crash will always happen, so should be
 - easily noticed. -}
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
bothHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> (Handle, Handle)
bothHandles (Just hin, Just hout, _, _) = (hin, hout)
bothHandles _ = error "expected bothHandles"

{- Debugging trace for a CreateProcess. -}
debugProcess :: CreateProcess -> IO ()
debugProcess p = do
	debugM "Utility.Process" $ unwords
		[ action ++ ":"
		, showCmd p
		, maybe "" show (env p)
		]
	where
		action
			| piped (std_in p) && piped (std_out p) = "chat"
			| piped (std_in p)                      = "feed"
			| piped (std_out p)                     = "read"
			| otherwise                             = "call"
		piped Inherit = False
		piped _ = True

{- Shows the command that a CreateProcess will run. -}
showCmd :: CreateProcess -> String
showCmd = go . cmdspec
	where
		go (ShellCommand s) = s
		go (RawCommand c ps) = c ++ " " ++ show ps

{- Wrappers for System.Process functions that do debug logging.
 - 
 - More could be added, but these are the only ones I usually need.
 -}

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
	debugProcess p
	System.Process.createProcess p

runInteractiveProcess
	:: FilePath	
	-> [String]	
	-> Maybe FilePath	
	-> Maybe [(String, String)]	
	-> IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveProcess f args c e = do
	debugProcess $ (proc f args)
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = CreatePipe
			}
	System.Process.runInteractiveProcess f args c e

readProcess
	:: FilePath	
	-> [String]	
	-> String	
	-> IO String
readProcess f args input = do
	debugProcess $ (proc f args) { std_out = CreatePipe }
	System.Process.readProcess f args input
