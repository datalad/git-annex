{- git-annex vector clocks
 -
 - We don't have a way yet to keep true distributed vector clocks.
 - The next best thing is a timestamp.
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.VectorClock where

import Data.Time.Clock.POSIX
import Control.Applicative
import Prelude

import Utility.QuickCheck

-- | Some very old logs did not have any time stamp at all;
-- Unknown is used for those.
data VectorClock = Unknown | VectorClock POSIXTime
	deriving (Eq, Ord, Show)

-- Unknown is oldest.
prop_VectorClock_sane :: Bool
prop_VectorClock_sane = Unknown < VectorClock 1

instance Arbitrary  VectorClock where
	arbitrary = VectorClock <$> arbitrary
