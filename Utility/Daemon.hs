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
			maybe noop (lockPidFile alreadyrunning) pidfile 
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

{- Locks the pid file, with an exclusive, non-blocking lock.
 - Runs an action on failure. On success, writes the pid to the file,
 - fully atomically. -}
lockPidFile :: IO () -> FilePath -> IO ()
lockPidFile onfailure file = do
	fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
	locked <- catchMaybeIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
	fd' <- openFd newfile ReadWrite (Just stdFileMode) defaultFileFlags
		{ trunc = True }
	locked' <- catchMaybeIO $ setLock fd' (WriteLock, AbsoluteSeek, 0, 0)
	case (locked, locked') of
		(Nothing, _) -> onfailure
		(_, Nothing) -> onfailure
		_ -> do
			_ <- fdWrite fd' =<< show <$> getProcessID
			renameFile newfile file
			closeFd fd
	where
		newfile = file ++ ".new"

{- Stops the daemon.
 -
 - The pid file is used to get the daemon's pid.
 -
 - To guard against a stale pid, check the lock of the pid file,
 - and compare the process that has it locked with the file content.
 -}
stopDaemon :: FilePath -> IO ()
stopDaemon pidfile = do
	fd <- openFd pidfile ReadOnly (Just stdFileMode) defaultFileFlags
	locked <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
	p <- readish <$> readFile pidfile
	case (locked, p) of
		(Nothing, _) -> noop
		(_, Nothing) -> noop
		(Just (pid, _), Just pid')
			| pid == pid' -> signalProcess sigTERM pid
			| otherwise -> error $
				"stale pid in " ++ pidfile ++ 
				" (got " ++ show pid' ++ 
				"; expected" ++ show pid ++ " )"
