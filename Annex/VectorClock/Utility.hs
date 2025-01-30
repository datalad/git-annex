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
import Utility.FileSystemEncoding

startVectorClock :: IO (IO CandidateVectorClock)
startVectorClock = go =<< getEnv "GIT_ANNEX_VECTOR_CLOCK"
  where
	go Nothing = timebased
	go (Just s) = case parsePOSIXTime (encodeBS s) of
		Just t -> return (pure (CandidateVectorClock t))
		Nothing -> timebased
	-- Avoid using fractional seconds in the CandidateVectorClock.
	-- This reduces the size of the packed git-annex branch by up
	-- to 8%. 
	--
	-- Due to the use of vector clocks, high resolution timestamps are
	-- not necessary to make clear which information is most recent when
	-- eg, a value is changed repeatedly in the same second. In such a
	-- case, the vector clock will be advanced to the next second after
	-- the last modification.
	timebased = return $
		CandidateVectorClock . truncateResolution 0 <$> getPOSIXTime
