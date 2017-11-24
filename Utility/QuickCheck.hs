{- QuickCheck with additional instances
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, CPP #-}

module Utility.QuickCheck
	( module X
	, module Utility.QuickCheck
	) where

import Test.QuickCheck as X
import Data.Time.Clock.POSIX
import System.Posix.Types
#if ! MIN_VERSION_QuickCheck(2,8,2)
import qualified Data.Map as M
import qualified Data.Set as S
#endif
import Control.Applicative
import Prelude

#if ! MIN_VERSION_QuickCheck(2,8,2)
instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (M.Map k v) where
	arbitrary = M.fromList <$> arbitrary

instance (Arbitrary v, Ord v) => Arbitrary (S.Set v) where
	arbitrary = S.fromList <$> arbitrary
#endif

{- Times before the epoch are excluded. -}
instance Arbitrary POSIXTime where
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
