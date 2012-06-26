{- git-annex assistant push tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pushes where

import Common.Annex

import Control.Concurrent.STM
import Data.Time.Clock
import qualified Data.Map as M

{- Track the most recent push failure for each remote. -}
type PushMap = M.Map Remote UTCTime
type FailedPushMap = TMVar PushMap

{- The TMVar starts empty, and is left empty when there are no
 - failed pushes. This way we can block until there are some failed pushes.
 -}
newFailedPushMap :: IO FailedPushMap
newFailedPushMap = atomically newEmptyTMVar

{- Blocks until there are failed pushes.
 - Returns Remotes whose pushes failed a given time duration or more ago.
 - (This may be an empty list.) -}
getFailedPushesBefore :: FailedPushMap -> NominalDiffTime -> IO [Remote]
getFailedPushesBefore v duration = do
	m <- atomically $ readTMVar v
	now <- getCurrentTime
	return $ M.keys $ M.filter (not . toorecent now) m
	where
		toorecent now time = now `diffUTCTime` time < duration

{- Modifies the map. -}
changeFailedPushMap :: FailedPushMap -> (PushMap -> PushMap) -> IO ()
changeFailedPushMap v a = atomically $
	store . a . fromMaybe M.empty =<< tryTakeTMVar v
	where
 		{- tryTakeTMVar empties the TMVar; refill it only if
		 - the modified map is not itself empty -}
		store m
			| m == M.empty = noop
			| otherwise = putTMVar v $! m
