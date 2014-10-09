{- Running a long or expensive batch operation niced.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Batch where

import Common

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

{- Makes a command be run by whichever of nice, ionice, and nocache
 - are available in the path. -}
type BatchCommandMaker = (String, [CommandParam]) -> (String, [CommandParam])

getBatchCommandMaker :: IO BatchCommandMaker
getBatchCommandMaker = do
#ifndef mingw32_HOST_OS
	nicers <- filterM (inPath . fst)
		[ ("nice", [])
#ifndef __ANDROID__
		-- Android's ionice does not allow specifying a command,
		-- so don't use it.
		, ("ionice", ["-c3"])
#endif
		, ("nocache", [])
		]
	return $ \(command, params) ->
		case nicers of
			[] -> (command, params)
			(first:rest) -> (fst first, map Param (snd first ++ concatMap (\p -> fst p : snd p) rest ++ [command]) ++ params)
#else
	return id
#endif

toBatchCommand :: (String, [CommandParam]) -> IO (String, [CommandParam])
toBatchCommand v = do
	batchmaker <- getBatchCommandMaker
	return $ batchmaker v

{- Runs a command in a way that's suitable for batch jobs that can be
 - interrupted.
 -
 - If the calling thread receives an async exception, it sends the
 - command a SIGTERM, and after the command finishes shuttting down,
 - it re-raises the async exception. -}
batchCommand :: String -> [CommandParam] -> IO Bool
batchCommand command params = batchCommandEnv command params Nothing

batchCommandEnv :: String -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
batchCommandEnv command params environ = do
	batchmaker <- getBatchCommandMaker
	let (command', params') = batchmaker (command, params)
	let p = proc command' $ toCommand params'
	(_, _, _, pid) <- createProcess $ p { env = environ }
	r <- E.try (waitForProcess pid) :: IO (Either E.SomeException ExitCode)
	case r of
		Right ExitSuccess -> return True
		Right _ -> return False
		Left asyncexception -> do
			terminateProcess pid
			void $ waitForProcess pid
			E.throwIO asyncexception
