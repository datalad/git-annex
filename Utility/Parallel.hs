{- parallel processes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Parallel where

import Common

{- Runs an action in parallel with a set of values.
 - Returns values that caused the action to fail. -}
inParallel :: (v -> IO ()) -> [v] -> IO [v]
inParallel a v = do
	pids <- mapM (forkProcess . a) v
	statuses <- mapM (getProcessStatus True False) pids
	return $ map fst $ filter failed $ zip v statuses
	where
		failed (_, status) = status /= Just (Exited ExitSuccess)
