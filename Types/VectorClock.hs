{- git-annex vector clocks
 -
 - Copyright 2017-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.VectorClock where

import Data.Time.Clock.POSIX

import Utility.QuickCheck

-- | Some very old logs did not have any time stamp at all;
-- Unknown is used for those.
data VectorClock = Unknown | VectorClock POSIXTime
	deriving (Eq, Ord, Show)

-- | This is a candidate value to use in a VectorClock. It
-- may not be suitable to use this, when a previously used VectorClock
-- is the same or higher.
data CandidateVectorClock = CandidateVectorClock POSIXTime

-- Unknown is oldest.
prop_VectorClock_sane :: Bool
prop_VectorClock_sane = Unknown < VectorClock 1

instance Arbitrary  VectorClock where
	arbitrary = VectorClock <$> arbitrary
