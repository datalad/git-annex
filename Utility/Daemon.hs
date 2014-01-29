{- daemon support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Daemon where

import Common
#ifndef mingw32_HOST_OS
import Utility.LogFile
#endif

#ifndef mingw32_HOST_OS
import System.Posix
import Control.Concurrent.Async
#else
import System.PosixCompat
#endif

{- Run an action as a daemon, with all output sent to a file descriptor.
 -
 - Can write its pid to a file, to guard against multiple instances
 - running and allow easy termination.
 -
 - When successful, does not return. -}
daemonize :: Fd -> Maybe FilePath -> Bool -> IO () -> IO ()
#ifndef mingw32_HOST_OS
daemonize logfd pidfile changedirectory a = do
	maybe noop checkalreadyrunning pidfile
	_ <- forkProcess child1
	out
  where
	checkalreadyrunning f = maybe noop (const $ alreadyRunning) 
		=<< checkDaemon f
	child1 = do
		_ <- createSession
		_ <- forkProcess child2
		out
	child2 = do
		maybe noop lockPidFile pidfile 
		when changedirectory $
			setCurrentDirectory "/"
		nullfd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
		redir nullfd stdInput
		redirLog logfd
		{- forkProcess masks async exceptions; unmask them inside
		 - the action. -}
		wait =<< asyncWithUnmask (\unmask -> unmask a)
		out
	out = exitImmediately ExitSuccess
#else
daemonize = error "daemonize is not implemented on Windows" -- TODO
#endif

{- Locks the pid file, with an exclusive, non-blocking lock.
 - Writes the pid to the file, fully atomically.
 - Fails if the pid file is already locked by another process. -}
lockPidFile :: FilePath -> IO ()
lockPidFile file = do
	createDirectoryIfMissing True (parentDir file)
#ifndef mingw32_HOST_OS
	fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
	locked <- catchMaybeIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
	fd' <- openFd newfile ReadWrite (Just stdFileMode) defaultFileFlags
		{ trunc = True }
	locked' <- catchMaybeIO $ setLock fd' (WriteLock, AbsoluteSeek, 0, 0)
	case (locked, locked') of
		(Nothing, _) -> alreadyRunning
		(_, Nothing) -> alreadyRunning
		_ -> do
			_ <- fdWrite fd' =<< show <$> getProcessID
			closeFd fd
#else
	writeFile newfile "-1"
#endif
	rename newfile file
  where
	newfile = file ++ ".new"

alreadyRunning :: IO ()
alreadyRunning = error "Daemon is already running."

{- Checks if the daemon is running, by checking that the pid file
 - is locked by the same process that is listed in the pid file.
 -
 - If it's running, returns its pid. -}
checkDaemon :: FilePath -> IO (Maybe ProcessID)
#ifndef mingw32_HOST_OS
checkDaemon pidfile = do
	v <- catchMaybeIO $
		openFd pidfile ReadOnly (Just stdFileMode) defaultFileFlags
	case v of
		Just fd -> do
			locked <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
			p <- readish <$> readFile pidfile
			closeFd fd `after` return (check locked p)
		Nothing -> return Nothing
  where
	check Nothing _ = Nothing
	check _ Nothing = Nothing
	check (Just (pid, _)) (Just pid')
		| pid == pid' = Just pid
		| otherwise = error $
			"stale pid in " ++ pidfile ++ 
			" (got " ++ show pid' ++ 
			"; expected " ++ show pid ++ " )"
#else
checkDaemon pidfile = maybe Nothing readish <$> catchMaybeIO (readFile pidfile)
#endif

{- Stops the daemon, safely. -}
stopDaemon :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
stopDaemon pidfile = go =<< checkDaemon pidfile
  where
	go Nothing = noop
	go (Just pid) = signalProcess sigTERM pid
#else
stopDaemon = error "stopDaemon is not implemented on Windows" -- TODO
#endif
