{- Tests for Utility.Path. Split into a separate module to avoid it needing
 - QuickCheck.
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
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
) where

import System.FilePath.ByteString
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Control.Applicative
import Prelude

import Utility.Path
import Utility.FileSystemEncoding
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
	-- Make the input an absolute path, since relPathDirToFileAbs
	-- needs absolute paths.
	p = pathSeparator `B.cons` dropDrive
		(toRawFilePath (fromTestableFilePath pt))

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
