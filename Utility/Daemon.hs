{- daemon functions
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Daemon where

import System.Posix
import System.Directory
import System.Exit
import Control.Monad

{- Run an action as a daemon, with all output sent to a file descriptor.
 -
 - Does not return. -}
daemonize :: Fd -> Bool -> IO () -> IO ()
daemonize logfd changedirectory a = do
	_ <- forkProcess child1
	end
	where
		child1 = do
			_ <- createSession
			_ <- forkProcess child2
			end
		child2 = do
			when changedirectory $
				setCurrentDirectory "/"
			nullfd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
			_ <- redir nullfd stdInput
			mapM_ (redir logfd) [stdOutput, stdError]
			closeFd logfd
			a
			end
		redir newh h = do
			closeFd h
			dupTo newh h
		end = exitImmediately ExitSuccess
