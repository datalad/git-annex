{- QuickCheck with additional instances
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utility.QuickCheck
	( module X
	, module Utility.QuickCheck
	) where

import Test.QuickCheck as X
import Data.Time.Clock.POSIX
import Data.Ratio
import System.Posix.Types
import Data.List.NonEmpty (NonEmpty(..))
import Prelude

{- Times before the epoch are excluded. Half with decimal and half without. -}
instance Arbitrary POSIXTime where
	arbitrary = do
		n <- nonNegative arbitrarySizedBoundedIntegral :: Gen Int
		d <- nonNegative arbitrarySizedIntegral
		withd <- arbitrary
		return $ if withd
			then fromIntegral n + fromRational (1 % max d 1)
			else fromIntegral n

{- Pids are never negative, or 0. -}
instance Arbitrary ProcessID where
	arbitrary = positive arbitrarySizedBoundedIntegral

{- Inodes are never negative. -}
instance Arbitrary FileID where
	arbitrary = nonNegative arbitrarySizedIntegral

{- File sizes are never negative. -}
instance Arbitrary FileOffset where
	arbitrary = nonNegative arbitrarySizedIntegral

instance Arbitrary l => Arbitrary (NonEmpty l) where
	arbitrary = (:|) <$> arbitrary <*> arbitrary

nonNegative :: (Num a, Ord a) => Gen a -> Gen a
nonNegative g = g `suchThat` (>= 0)

positive :: (Num a, Ord a) => Gen a -> Gen a
positive g = g `suchThat` (> 0)
