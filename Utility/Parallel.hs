{- parallel processes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Parallel where

import Common

import System.Posix.Process

{- Runs an action in parallel with a set of values.
 - Returns the values partitioned into ones with which the action succeeded,
 - and ones with which it failed. -}
inParallel :: (v -> IO ()) -> [v] -> IO ([v], [v])
inParallel a l = do
	pids <- mapM (forkProcess . a) l
	statuses <- mapM (getProcessStatus True False) pids
	return $ reduce $ partition (succeeded . snd) $ zip l statuses
	where
		succeeded v = v == Just (Exited ExitSuccess)
		reduce (x,y) = (map fst x, map fst y)
