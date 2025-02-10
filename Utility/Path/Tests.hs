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

import Data.List
import Data.Maybe
import Control.Applicative
import Prelude

import Common
import Utility.QuickCheck
import qualified Utility.OsString as OS

prop_upFrom_basics :: TestableFilePath -> Bool
prop_upFrom_basics tdir
	| dir == "/" = p == Nothing
	| otherwise = p /= Just dir
  where
	p = fromOsPath <$> upFrom (toOsPath dir)
	dir = fromTestableFilePath tdir

prop_relPathDirToFileAbs_basics :: TestableFilePath -> Bool
prop_relPathDirToFileAbs_basics pt = and
	[ relPathDirToFileAbs p (p </> literalOsPath "bar") == literalOsPath "bar"
	, relPathDirToFileAbs (p </> literalOsPath "bar") p == literalOsPath ".."
	, relPathDirToFileAbs p p == literalOsPath ""
	]
  where
	-- relPathDirToFileAbs needs absolute paths, so make the path
	-- absolute by adding a path separator to the front.
	p = pathSeparator `OS.cons` relf
	-- Make the input a relative path. On windows, make sure it does
	-- not contain anything that looks like a drive letter.
	relf = OS.dropWhile isPathSeparator $
		OS.filter (not . skipchar) $
		toOsPath (fromTestableFilePath pt)
	skipchar b = b == unsafeFromChar ':'

prop_relPathDirToFileAbs_regressionTest :: Bool
prop_relPathDirToFileAbs_regressionTest = same_dir_shortcurcuits_at_difference
  where
	{- Two paths have the same directory component at the same
	 - location, but it's not really the same directory.
	 - Code used to get this wrong. -}
	same_dir_shortcurcuits_at_difference =
		relPathDirToFileAbs (mkp [fromOsPath (pathSeparator `OS.cons` literalOsPath "tmp"), "r", "lll", "xxx", "yyy", "18"])
			(mkp [fromOsPath (pathSeparator `OS.cons` literalOsPath "tmp"), "r", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"])
				== mkp ["..", "..", "..", "..", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"]
	  where
		mkp = joinPath . map literalOsPath

prop_dirContains_regressionTest :: Bool
prop_dirContains_regressionTest = and
	[ not $ dc "." ".."
	, not $ dc ".." "../.."
	, dc "." "foo"
	, dc "." "."
	, dc ".." ".."
	, dc "../.." "../.."
	, dc "." "./foo"
	, dc ".." "../foo"
	, dc "../.." "../foo"
	, dc "../.." "../../foo"
	, not $ dc "../.." "../../.."
	]
  where
	dc x y = dirContains (literalOsPath x) (literalOsPath y)
