{- Running a long or expensive batch operation niced.
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Batch (
	batch,
	BatchCommandMaker,
	nonBatchCommandMaker,
	getBatchCommandMaker,
	toBatchCommand,
	batchCommand,
	batchCommandEnv,
) where

import Common

#if defined(linux_HOST_OS)
import Control.Concurrent.Async
import System.Posix.Process
#endif

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
#if defined(linux_HOST_OS)
batch a = wait =<< batchthread
  where
	batchthread = asyncBound $ do
		setProcessPriority 0 maxNice
		a
	maxNice = 19
#else
batch a = a
#endif

{- Makes a command be run by whichever of nice, ionice, and nocache
 - are available in the path. -}
type BatchCommandMaker = (String, [CommandParam]) -> (String, [CommandParam])

nonBatchCommandMaker :: BatchCommandMaker
nonBatchCommandMaker = id

getBatchCommandMaker :: IO BatchCommandMaker
getBatchCommandMaker = do
#ifndef mingw32_HOST_OS
	nicers <- filterM (inPath . fst)
		[ ("nice", [])
		, ("ionice", ["-c3"])
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
 - interrupted. -}
batchCommand :: String -> [CommandParam] -> IO Bool
batchCommand command params = batchCommandEnv command params Nothing

batchCommandEnv :: String -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
batchCommandEnv command params environ = do
	batchmaker <- getBatchCommandMaker
	let (command', params') = batchmaker (command, params)
	boolSystemEnv command' params' environ
