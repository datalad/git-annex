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
			maybe noop lockPidFile pidfile
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
		out = exitImmediately ExitSuccess

lockPidFile :: FilePath -> IO ()
lockPidFile file = void $ do
	fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
	catchIO
		(setLock fd (WriteLock, AbsoluteSeek, 0, 0))
		(const $ error "Daemon is already running.")
	fdWrite fd =<< show <$> getProcessID
