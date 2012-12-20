{- QuickCheck instances
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utility.QuickCheck where

import Test.QuickCheck
import Data.Time.Clock.POSIX
import System.Posix.Types

{- Times before the epoch are excluded. -}
instance Arbitrary POSIXTime where
	arbitrary = nonNegative arbitrarySizedIntegral

instance Arbitrary EpochTime where
	arbitrary = nonNegative arbitrarySizedIntegral

{- Pids are never negative, or 0. -}
instance Arbitrary ProcessID where
	arbitrary = arbitrarySizedBoundedIntegral `suchThat` (> 0)

{- Inodes are never negative. -}
instance Arbitrary FileID where
	arbitrary = nonNegative arbitrarySizedIntegral

{- File sizes are never negative. -}
instance Arbitrary FileOffset where
	arbitrary = nonNegative arbitrarySizedIntegral

nonNegative :: (Num a, Ord a) => Gen a -> Gen a
nonNegative g = g `suchThat` (>= 0)
