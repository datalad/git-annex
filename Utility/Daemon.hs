{- daemon support
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Daemon (
#ifndef mingw32_HOST_OS
	daemonize,
#endif
	foreground,
	checkDaemon,
	stopDaemon,
) where

import Common
import Utility.PID
#ifndef mingw32_HOST_OS
import Utility.LogFile
import Utility.Env
#else
import System.Win32.Process (terminateProcessById)
import Utility.LockFile
#endif

#ifndef mingw32_HOST_OS
import System.Posix hiding (getEnv, getEnvironment)
#endif

#ifndef mingw32_HOST_OS
{- Run an action as a daemon, with all output sent to a file descriptor,
 - and in a new session.
 -
 - Can write its pid to a file.
 -
 - This does not double-fork to background, because forkProcess is
 - rather fragile and highly unused in haskell programs, so likely to break.
 - Instead, it runs the cmd with provided params, in the background,
 - which the caller should arrange to run this again.
 -}
daemonize :: String -> [CommandParam] -> IO Fd -> Maybe FilePath -> Bool -> IO () -> IO ()
daemonize cmd params openlogfd pidfile changedirectory a = do
	maybe noop checkalreadyrunning pidfile
	getEnv envvar >>= \case
		Just s | s == cmd -> do
			maybe noop lockPidFile pidfile 
			a
		_ -> do
			nullfd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
			redir nullfd stdInput
			redirLog =<< openlogfd
			environ <- getEnvironment
			_ <- createProcess $
				(proc cmd (toCommand params))
				{ env = Just (addEntry envvar cmd environ)
				, create_group = True
				, new_session = True
				, cwd = if changedirectory then Just "/" else Nothing
				}
			return ()
  where
	checkalreadyrunning f = maybe noop (const alreadyRunning) 
		=<< checkDaemon f
	envvar = "DAEMONIZED"
#endif

{- To run an action that is normally daemonized in the foreground. -}
#ifndef mingw32_HOST_OS
foreground :: IO Fd -> Maybe FilePath -> IO () -> IO ()
foreground openlogfd pidfile a = do
#else
foreground :: Maybe FilePath -> IO () -> IO ()
foreground pidfile a = do
#endif
	maybe noop lockPidFile pidfile
#ifndef mingw32_HOST_OS
	_ <- tryIO createSession
	redirLog =<< openlogfd
#endif
	a
#ifndef mingw32_HOST_OS
	exitImmediately ExitSuccess
#else
	exitWith ExitSuccess
#endif

{- Locks the pid file, with an exclusive, non-blocking lock,
 - and leaves it locked on return.
 -
 - Writes the pid to the file, fully atomically.
 - Fails if the pid file is already locked by another process. -}
lockPidFile :: FilePath -> IO ()
lockPidFile pidfile = do
#ifndef mingw32_HOST_OS
	fd <- openFd pidfile ReadWrite (Just stdFileMode) defaultFileFlags
	locked <- catchMaybeIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
	fd' <- openFd newfile ReadWrite (Just stdFileMode) defaultFileFlags
		{ trunc = True }
	locked' <- catchMaybeIO $ setLock fd' (WriteLock, AbsoluteSeek, 0, 0)
	case (locked, locked') of
		(Nothing, _) -> alreadyRunning
		(_, Nothing) -> alreadyRunning
		_ -> do
			_ <- fdWrite fd' =<< show <$> getPID
			closeFd fd
	rename newfile pidfile
  where
	newfile = pidfile ++ ".new"
#else
	{- Not atomic on Windows, oh well. -}
	unlessM (isNothing <$> checkDaemon pidfile)
		alreadyRunning
	pid <- getPID
	writeFile pidfile (show pid)
	lckfile <- winLockFile pid pidfile
	writeFile (fromRawFilePath lckfile) ""
	void $ lockExclusive lckfile
#endif

alreadyRunning :: IO ()
alreadyRunning = giveup "Daemon is already running."

{- Checks if the daemon is running, by checking that the pid file
 - is locked by the same process that is listed in the pid file.
 -
 - If it's running, returns its pid. -}
checkDaemon :: FilePath -> IO (Maybe PID)
#ifndef mingw32_HOST_OS
checkDaemon pidfile = bracket setup cleanup go
  where
	setup = catchMaybeIO $
		openFd pidfile ReadOnly (Just stdFileMode) defaultFileFlags
	cleanup (Just fd) = closeFd fd
	cleanup Nothing = return ()
	go (Just fd) = catchDefaultIO Nothing $ do
		locked <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
		p <- readish <$> readFile pidfile
		return (check locked p)
	go Nothing = return Nothing

	check Nothing _ = Nothing
	check _ Nothing = Nothing
	check (Just (pid, _)) (Just pid')
		| pid == pid' = Just pid
		| otherwise = giveup $
			"stale pid in " ++ pidfile ++ 
			" (got " ++ show pid' ++ 
			"; expected " ++ show pid ++ " )"
#else
checkDaemon pidfile = maybe (return Nothing) (check . readish)
	=<< catchMaybeIO (readFile pidfile)
  where
	check Nothing = return Nothing
	check (Just pid) = do
		v <- lockShared =<< winLockFile pid pidfile
		case v of
			Just h -> do
				dropLock h
				return Nothing
			Nothing -> return (Just pid)
#endif

{- Stops the daemon, safely. -}
stopDaemon :: FilePath -> IO ()
stopDaemon pidfile = go =<< checkDaemon pidfile
  where
	go Nothing = noop
	go (Just pid) =
#ifndef mingw32_HOST_OS
		signalProcess sigTERM pid
#else
		terminateProcessById pid
#endif

{- Windows locks a lock file that corresponds with the pid of the process.
 - This allows changing the process in the pid file and taking a new lock
 - when eg, restarting the daemon.
 -}
#ifdef mingw32_HOST_OS
winLockFile :: PID -> FilePath -> IO RawFilePath
winLockFile pid pidfile = do
	cleanstale
	return $ toRawFilePath $ prefix ++ show pid ++ suffix
  where
	prefix = pidfile ++ "."
	suffix = ".lck"
	cleanstale = mapM_ (void . tryIO . removeFile) =<<
		(filter iswinlockfile <$> dirContents (fromRawFilePath (parentDir (toRawFilePath pidfile))))
	iswinlockfile f = suffix `isSuffixOf` f && prefix `isPrefixOf` f
#endif
