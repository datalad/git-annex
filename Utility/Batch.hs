{- Running a long or expensive batch operation niced.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Batch where

#if defined(__LINUX__) || defined(__ANDROID__)
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
#if defined(__LINUX__) || defined(__ANDROID__)
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
