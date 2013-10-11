{- Running a long or expensive batch operation niced.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Batch where

import Common
import qualified Build.SysConfig

#if defined(linux_HOST_OS) || defined(__ANDROID__)
import Control.Concurrent.Async
import System.Posix.Process
#endif
import qualified Control.Exception as E

{- Runs an operation, at batch priority.
 -
 - This is done by running it in a bound thread, which on Linux can be set
 - to have a different nice level than the rest of the program. Note that
 - due to running in a bound thread, some operations may be more expensive
 - to perform. Also note that if the action calls forkIO or forkOS itself,
 - that will make a new thread that does not have the batch priority.
 -
 - POSIX threads do not support separate nice levels, so on other operating
 - systems, the action is simply ran.
 -}
batch :: IO a -> IO a
#if defined(linux_HOST_OS) || defined(__ANDROID__)
batch a = wait =<< batchthread
  where
  	batchthread = asyncBound $ do
		setProcessPriority 0 maxNice
		a
#else
batch a = a
#endif

maxNice :: Int
maxNice = 19

{- Runs a command in a way that's suitable for batch jobs.
 - The command is run niced. If the calling thread receives an async
 - exception, it sends the command a SIGTERM, and after the command
 - finishes shuttting down, it re-raises the async exception. -}
batchCommand :: String -> [CommandParam] -> IO Bool
batchCommand command params = do
	(_, _, _, pid) <- createProcess $ proc "sh"
		[ "-c"
		, "exec " ++ nicedcommand
		]
	r <- E.try (waitForProcess pid) :: IO (Either E.SomeException ExitCode)
	case r of
		Right ExitSuccess -> return True
		Right _ -> return False
		Left asyncexception -> do
			terminateProcess pid
			void $ waitForProcess pid
			E.throwIO asyncexception
  where
  	commandline = unwords $ map shellEscape $ command : toCommand params
  	nicedcommand
		| Build.SysConfig.nice = "nice " ++ commandline
		| otherwise = commandline
