{- git-annex vector clock utilities
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.VectorClock.Utility where

import Data.Time.Clock.POSIX

import Types.VectorClock
import Utility.Env
import Utility.TimeStamp

startVectorClock :: IO (IO VectorClock)
startVectorClock = go =<< getEnv "GIT_ANNEX_VECTOR_CLOCK"
  where
	go Nothing = timebased
	go (Just s) = case parsePOSIXTime s of
		Just t -> return (pure (VectorClock t))
		Nothing -> timebased
	timebased = return (VectorClock <$> getPOSIXTime)
