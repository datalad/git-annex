{- daemon support
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
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
import Utility.OpenFd
#else
import System.Win32.Process (terminateProcessById)
import Utility.LockFile
import qualified Utility.OsString as OS
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
daemonize :: String -> [CommandParam] -> IO Fd -> Maybe OsPath -> Bool -> IO () -> IO ()
daemonize cmd params openlogfd pidfile changedirectory a = do
	maybe noop checkalreadyrunning pidfile
	getEnv envvar >>= \case
		Just s | s == cmd -> do
			maybe noop lockPidFile pidfile 
			a
		_ -> do
			nullfd <- openFdWithMode (toRawFilePath "/dev/null") ReadOnly Nothing defaultFileFlags 
				(CloseOnExecFlag True)
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
foreground :: IO Fd -> Maybe OsPath -> IO () -> IO ()
foreground openlogfd pidfile a = do
#else
foreground :: Maybe OsPath -> IO () -> IO ()
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
 - and leaves it locked on return. The lock file is not closed on exec, so
 - when daemonize runs the process again, it inherits it.
 -
 - Writes the pid to the file, fully atomically.
 - Fails if the pid file is already locked by another process. -}
lockPidFile :: OsPath -> IO ()
lockPidFile pidfile = do
#ifndef mingw32_HOST_OS
	fd <- openFdWithMode (fromOsPath pidfile) ReadWrite (Just stdFileMode) defaultFileFlags
		(CloseOnExecFlag False)
	locked <- catchMaybeIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
	fd' <- openFdWithMode (fromOsPath newfile) ReadWrite (Just stdFileMode)
		(defaultFileFlags { trunc = True })
		(CloseOnExecFlag True)
	locked' <- catchMaybeIO $ setLock fd' (WriteLock, AbsoluteSeek, 0, 0)
	case (locked, locked') of
		(Nothing, _) -> alreadyRunning
		(_, Nothing) -> alreadyRunning
		_ -> do
			_ <- fdWrite fd' =<< show <$> getPID
			closeFd fd
	renameFile newfile pidfile
  where
	newfile = pidfile <> literalOsPath ".new"
#else
	{- Not atomic on Windows, oh well. -}
	unlessM (isNothing <$> checkDaemon pidfile)
		alreadyRunning
	pid <- getPID
	writeFile (fromOsPath pidfile) (show pid)
	lckfile <- winLockFile pid pidfile
	writeFile (fromOsPath lckfile) ""
	void $ lockExclusive lckfile
#endif

alreadyRunning :: IO ()
alreadyRunning = giveup "Daemon is already running."

{- Checks if the daemon is running, by checking that the pid file
 - is locked by the same process that is listed in the pid file.
 -
 - If it's running, returns its pid. -}
checkDaemon :: OsPath -> IO (Maybe PID)
#ifndef mingw32_HOST_OS
checkDaemon pidfile = bracket setup cleanup go
  where
	setup = catchMaybeIO $
		openFdWithMode (fromOsPath pidfile) ReadOnly
			(Just stdFileMode) 
			defaultFileFlags
			(CloseOnExecFlag True)
	cleanup (Just fd) = closeFd fd
	cleanup Nothing = return ()
	go (Just fd) = catchDefaultIO Nothing $ do
		locked <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
		p <- readish <$> readFile (fromOsPath pidfile)
		return (check locked p)
	go Nothing = return Nothing

	check Nothing _ = Nothing
	check _ Nothing = Nothing
	check (Just (pid, _)) (Just pid')
		| pid == pid' = Just pid
		| otherwise = giveup $
			"stale pid in " ++ fromOsPath pidfile ++ 
			" (got " ++ show pid' ++ 
			"; expected " ++ show pid ++ " )"
#else
checkDaemon pidfile = maybe (return Nothing) (check . readish)
	=<< catchMaybeIO (readFile (fromOsPath pidfile))
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
stopDaemon :: OsPath -> IO ()
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
winLockFile :: PID -> OsPath -> IO OsPath
winLockFile pid pidfile = do
	cleanstale
	return $ prefix <> toOsPath (show pid) <> suffix
  where
	prefix = pidfile <> literalOsPath "."
	suffix = literalOsPath ".lck"
	cleanstale = mapM_ (void . tryIO . removeFile) =<<
		(filter iswinlockfile <$> dirContents (parentDir pidfile))
	iswinlockfile f = suffix `OS.isSuffixOf` f && prefix `OS.isPrefixOf` f
#endif
