{- git-annex assistant push tracking
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Pushes where

import Assistant.Common
import Assistant.Types.Pushes

import Control.Concurrent.STM
import Data.Time.Clock
import qualified Data.Map as M

{- Blocks until there are failed pushes.
 - Returns Remotes whose pushes failed a given time duration or more ago.
 - (This may be an empty list.) -}
getFailedPushesBefore :: NominalDiffTime -> FailedPushMap -> Assistant [Remote]
getFailedPushesBefore duration v = liftIO $ do
	m <- atomically $ readTMVar v
	now <- getCurrentTime
	return $ M.keys $ M.filter (not . toorecent now) m
  where
	toorecent now time = now `diffUTCTime` time < duration

{- Modifies the map. -}
changeFailedPushMap :: FailedPushMap -> (PushMap -> PushMap) -> Assistant ()
changeFailedPushMap v f = liftIO $ atomically $
	store . f . fromMaybe M.empty =<< tryTakeTMVar v
  where
	{- tryTakeTMVar empties the TMVar; refill it only if
	 - the modified map is not itself empty -}
	store m
		| m == M.empty = noop
		| otherwise = putTMVar v $! m
