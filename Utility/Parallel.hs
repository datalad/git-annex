{- parallel processing via threads
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Parallel where

import Common

import Control.Concurrent

{- Runs an action in parallel with a set of values, in a set of threads.
 - In order for the actions to truely run in parallel, requires GHC's
 - threaded runtime, 
 -
 - Returns the values partitioned into ones with which the action succeeded,
 - and ones with which it failed. -}
inParallel :: (v -> IO Bool) -> [v] -> IO ([v], [v])
inParallel a l = do
	mvars <- mapM thread l
	statuses <- mapM takeMVar mvars
	return $ reduce $ partition snd $ zip l statuses
  where
	reduce (x,y) = (map fst x, map fst y)
	thread v = do
		mvar <- newEmptyMVar
		_ <- forkIO $ do
			r <- try (a v) :: IO (Either SomeException Bool)
			case r of
				Left _ -> putMVar mvar False
				Right b -> putMVar mvar b
		return mvar
