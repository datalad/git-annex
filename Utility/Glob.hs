{-# LANGUAGE PackageImports #-}

{- file globbing
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Glob (
	Glob,
	GlobCase(..),
	compileGlob,
	matchGlob
) where

import Utility.Exception

import "regex-tdfa" Text.Regex.TDFA
import "regex-tdfa" Text.Regex.TDFA.String
import Data.Char

newtype Glob = Glob Regex

data GlobCase = CaseSensative | CaseInsensative

{- Compiles a glob to a regex, that can be repeatedly used. -}
compileGlob :: String -> GlobCase -> Glob
compileGlob glob globcase = Glob $
	case compile (defaultCompOpt {caseSensitive = casesentitive}) defaultExecOpt regex of
		Right r -> r
		Left _ -> giveup $ "failed to compile regex: " ++ regex
  where
	regex = '^' : wildToRegex glob ++ "$"
	casesentitive = case globcase of
		CaseSensative -> True
		CaseInsensative -> False

wildToRegex :: String -> String
wildToRegex = concat . go
  where
	go [] = []
	go ('*':xs) = ".*" : go xs
	go ('?':xs) = "." : go xs
	go ('[':'!':xs) = "[^" : inpat xs
	go ('[':xs) = "[" : inpat xs
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
