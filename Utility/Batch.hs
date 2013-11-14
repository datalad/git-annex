{- Running a long or expensive batch operation niced.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Batch where

import Common
#ifndef mingw32_HOST_OS
import qualified Build.SysConfig
#endif

#if defined(linux_HOST_OS) || defined(__ANDROID__)
import Control.Concurrent.Async
import System.Posix.Process
#endif
import qualified Control.Exception as E
import System.Process (env)

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

{- Converts a command to run niced. -}
toBatchCommand :: (String, [CommandParam]) -> (String, [CommandParam])
toBatchCommand (command, params) = (command', params')
  where
#ifndef mingw32_HOST_OS
	commandline = unwords $ map shellEscape $ command : toCommand params
	nicedcommand
		| Build.SysConfig.nice = "nice " ++ commandline
		| otherwise = commandline
	command' = "sh"
	params' =
		[ Param "-c"
		, Param $ "exec " ++ nicedcommand
		]
#else
	command' = command
	params' = params
#endif

{- Runs a command in a way that's suitable for batch jobs that can be
 - interrupted.
 -
 - The command is run niced. If the calling thread receives an async
 - exception, it sends the command a SIGTERM, and after the command
 - finishes shuttting down, it re-raises the async exception. -}
batchCommand :: String -> [CommandParam] -> IO Bool
batchCommand command params = batchCommandEnv command params Nothing

batchCommandEnv :: String -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
batchCommandEnv command params environ = do
	(_, _, _, pid) <- createProcess $ p { env = environ }
	r <- E.try (waitForProcess pid) :: IO (Either E.SomeException ExitCode)
	case r of
		Right ExitSuccess -> return True
		Right _ -> return False
		Left asyncexception -> do
			terminateProcess pid
			void $ waitForProcess pid
			E.throwIO asyncexception
  where
  	(command', params') = toBatchCommand (command, params)
  	p = proc command' $ toCommand params'

