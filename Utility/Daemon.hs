{- daemon support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Daemon where

import Common

import System.Posix

{- Run an action as a daemon, with all output sent to a file descriptor.
 -
 - Can write its pid to a file, to guard against multiple instances
 - running and allow easy termination.
 -
 - When successful, does not return. -}
daemonize :: Fd -> Maybe FilePath -> Bool -> IO () -> IO ()
daemonize logfd pidfile changedirectory a = do
	_ <- forkProcess child1
	out
	where
		child1 = do
			_ <- createSession
			_ <- forkProcess child2
			out
		child2 = do
			maybe noop (lockPidFile True alreadyrunning) pidfile 
			when changedirectory $
				setCurrentDirectory "/"
			nullfd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
			_ <- redir nullfd stdInput
			mapM_ (redir logfd) [stdOutput, stdError]
			closeFd logfd
			a
			out
		redir newh h = do
			closeFd h
			dupTo newh h
		alreadyrunning = error "Daemon is already running."
		out = exitImmediately ExitSuccess

lockPidFile :: Bool -> IO () -> FilePath -> IO ()
lockPidFile write onfailure file = do
	fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
	locked <- catchMaybeIO $ setLock fd (locktype, AbsoluteSeek, 0, 0)
	case locked of
		Nothing -> onfailure
		_ -> when write $ void $
			fdWrite fd =<< show <$> getProcessID
	where
		locktype
			| write = WriteLock
			| otherwise = ReadLock

{- Stops the daemon.
 -
 - The pid file is used to get the daemon's pid.
 -
 - To guard against a stale pid, try to take a nonblocking shared lock
 - of the pid file. If this *fails*, the daemon must be running,
 - and have the exclusive lock, so the pid file is trustworthy.
 -}
stopDaemon :: FilePath -> IO ()
stopDaemon pidfile = lockPidFile False go pidfile
	where
		go = do
			pid <- readish <$> readFile pidfile
			maybe noop (signalProcess sigTERM) pid
