{- QuickCheck with additional instances
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utility.QuickCheck
	( module X
	, TestableString
	, fromTestableString
	, TestableFilePath(..)
	, fromTestableFilePath
	, nonNegative
	, positive
	) where

import Test.QuickCheck as X
import Data.Time.Clock.POSIX
import Data.Ratio
import Data.Char
import System.Posix.Types
import Data.List.NonEmpty (NonEmpty(..))
import Prelude

{- A String, but Arbitrary is limited to ascii.
 -
 - When in a non-utf8 locale, String does not normally contain any non-ascii
 - characters, except for ones in surrogate plane. Converting a string that
 - does contain other unicode characters to a ByteString using the
 - filesystem encoding (see GHC.IO.Encoding) will throw an exception,
 - so use this instead to avoid quickcheck tests breaking unncessarily.
 -} 
newtype TestableString = TestableString
	{ fromTestableString :: String }
	deriving (Show)

instance Arbitrary TestableString where
	arbitrary = TestableString . filter isAscii <$> arbitrary

{- FilePath constrained to not be the empty string, not contain a NUL, 
 - and contain only ascii.
 -
 - No real-world filename can be empty or contain a NUL. So code can
 - well be written that assumes that and using this avoids quickcheck
 - tests breaking unncessarily.
 -} 
newtype TestableFilePath = TestableFilePath
	{ fromTestableFilePath :: FilePath }
	deriving (Show)

instance Arbitrary TestableFilePath where
	arbitrary = (TestableFilePath . fromTestableString <$> arbitrary)
		`suchThat` (not . null . fromTestableFilePath)
		`suchThat` (not . any (== '\NUL') . fromTestableFilePath)

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
