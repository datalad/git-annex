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
	pidfd <- lockpidfile
	_ <- forkProcess $ child1 pidfd
	out
	where
		child1 pidfd = do
			_ <- createSession
			_ <- forkProcess $ child2 pidfd
			out
		child2 pidfd = do
			writepidfile pidfd
			when changedirectory $
				setCurrentDirectory "/"
			nullfd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
			_ <- redir nullfd stdInput
			mapM_ (redir logfd) [stdOutput, stdError]
			closeFd logfd
			a
			out
		lockpidfile = case pidfile of
			Just file -> do
				fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
				return $ Just fd
			Nothing -> return Nothing
		writepidfile pidfd = 
			case pidfd of
				Just fd -> void $
					fdWrite fd =<< show <$> getProcessID
				Nothing -> return ()
		redir newh h = do
			closeFd h
			dupTo newh h
		out = exitImmediately ExitSuccess
