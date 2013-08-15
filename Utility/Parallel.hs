{- parallel processing via threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Parallel where

import Common

import Control.Concurrent
import Control.Exception

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
