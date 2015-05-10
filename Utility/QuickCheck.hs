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
import System.Posix.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Prelude

instance (Arbitrary k, Arbitrary v, Eq k, Ord k) => Arbitrary (M.Map k v) where
	arbitrary = M.fromList <$> arbitrary

instance (Arbitrary v, Eq v, Ord v) => Arbitrary (S.Set v) where
	arbitrary = S.fromList <$> arbitrary

{- Times before the epoch are excluded. -}
instance Arbitrary POSIXTime where
	arbitrary = fromInteger <$> nonNegative arbitrarySizedIntegral

instance Arbitrary EpochTime where
	arbitrary = fromInteger <$> nonNegative arbitrarySizedIntegral

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

positive :: (Num a, Ord a) => Gen a -> Gen a
positive g = g `suchThat` (> 0)
