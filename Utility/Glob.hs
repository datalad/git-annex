{- file globbing
 -
 - Copyright 2014-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Utility.Glob (
	Glob,
	GlobCase(..),
	GlobFilePath(..),
	compileGlob,
	matchGlob
) where

import Utility.Exception

import "regex-tdfa" Text.Regex.TDFA
import "regex-tdfa" Text.Regex.TDFA.String
import Data.Char

newtype Glob = Glob Regex

data GlobCase = CaseSensitive | CaseInsensitive

-- Is the glob being used to match filenames? 
--
-- When matching filenames,
-- a single path separator (eg /) in the glob will match any
-- number of path separators in the filename.
-- And on Windows, both / and \ are used as path separators, so compile
-- the glob to a regexp that matches either path separator.
newtype GlobFilePath = GlobFilePath Bool

{- Compiles a glob to a regex, that can be repeatedly used. -}
compileGlob :: String -> GlobCase -> GlobFilePath -> Glob
compileGlob glob globcase globfilepath = Glob $
	case compile (defaultCompOpt {caseSensitive = casesentitive}) defaultExecOpt regex of
		Right r -> r
		Left _ -> giveup $ "failed to compile regex: " ++ regex
  where
	regex = '^' : wildToRegex globfilepath glob ++ "$"
	casesentitive = case globcase of
		CaseSensitive -> True
		CaseInsensitive -> False

wildToRegex :: GlobFilePath -> String -> String
wildToRegex (GlobFilePath globfile) = concat . go
  where
	go [] = []
	go ('*':xs) = ".*" : go xs
	go ('?':xs) = "." : go xs
	go ('[':'!':xs) = "[^" : inpat xs
	go ('[':xs) = "[" : inpat xs
#ifdef mingw32_HOST_OS
	go ('/':xs) | globfile = "[/\\]+" : go xs
	go ('\\':xs) | globfile = "[/\\]+" : go xs
#else
	go ('/':xs) | globfile = "[/]+" : go xs
	go ('\\':xs) | globfile = "[\\]+" : go xs
#endif
	go (x:xs)
		| isDigit x || isAlpha x = [x] : go xs
		| otherwise = esc x : go xs

	inpat [] = []
	inpat (x:xs) = case x of
		']' -> "]" : go xs
		'\\' -> esc x : inpat xs
		_ -> [x] : inpat xs

	esc c = ['\\', c]

matchGlob :: Glob -> String -> Bool
matchGlob (Glob regex) val = 
	case execute regex val of
		Right (Just _) -> True
		_ -> False
