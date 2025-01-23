{- Tests for Utility.Path. Split into a separate module to avoid it needing
 - QuickCheck.
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Path.Tests (
	prop_upFrom_basics,
	prop_relPathDirToFileAbs_basics,
	prop_relPathDirToFileAbs_regressionTest,
	prop_dirContains_regressionTest,
) where

import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative
import Prelude

import Common
import Utility.Path
import Utility.QuickCheck

prop_upFrom_basics :: TestableFilePath -> Bool
prop_upFrom_basics tdir
	| dir == "/" = p == Nothing
	| otherwise = p /= Just dir
  where
	p = fromRawFilePath <$> upFrom (toRawFilePath dir)
	dir = fromTestableFilePath tdir

prop_relPathDirToFileAbs_basics :: TestableFilePath -> Bool
prop_relPathDirToFileAbs_basics pt = and
	[ relPathDirToFileAbs p (p </> "bar") == "bar"
	, relPathDirToFileAbs (p </> "bar") p == ".."
	, relPathDirToFileAbs p p == ""
	]
  where
	-- relPathDirToFileAbs needs absolute paths, so make the path
	-- absolute by adding a path separator to the front.
	p = pathSeparator `B.cons` relf
	-- Make the input a relative path. On windows, make sure it does
	-- not contain anything that looks like a drive letter.
	relf = B.dropWhile isPathSeparator $
		B.filter (not . skipchar) $
		toRawFilePath (fromTestableFilePath pt)
	skipchar b = b == (fromIntegral (ord ':'))

prop_relPathDirToFileAbs_regressionTest :: Bool
prop_relPathDirToFileAbs_regressionTest = same_dir_shortcurcuits_at_difference
  where
	{- Two paths have the same directory component at the same
	 - location, but it's not really the same directory.
	 - Code used to get this wrong. -}
	same_dir_shortcurcuits_at_difference =
		relPathDirToFileAbs (joinPath [pathSeparator `B.cons` "tmp", "r", "lll", "xxx", "yyy", "18"])
			(joinPath [pathSeparator `B.cons` "tmp", "r", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"])
				== joinPath ["..", "..", "..", "..", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"]

prop_dirContains_regressionTest :: Bool
prop_dirContains_regressionTest = and
	[ not $ dirContains "." ".."
	, not $ dirContains ".." "../.."
	, dirContains "." "foo"
	, dirContains "." "."
	, dirContains ".." ".."
	, dirContains "../.." "../.."
	, dirContains "." "./foo"
	, dirContains ".." "../foo"
	, dirContains "../.." "../foo"
	, dirContains "../.." "../../foo"
	, not $ dirContains "../.." "../../.."
	]
