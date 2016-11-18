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

import System.Path.WildMatch

import "regex-tdfa" Text.Regex.TDFA
import "regex-tdfa" Text.Regex.TDFA.String

newtype Glob = Glob Regex

data GlobCase = CaseSensative | CaseInsensative

{- Compiles a glob to a regex, that can be repeatedly used. -}
compileGlob :: String -> GlobCase -> Glob
compileGlob glob globcase = Glob $
	case compile (defaultCompOpt {caseSensitive = casesentitive}) defaultExecOpt regex of
		Right r -> r
		Left _ -> giveup $ "failed to compile regex: " ++ regex
  where
	regex = '^':wildToRegex glob
	casesentitive = case globcase of
		CaseSensative -> True
		CaseInsensative -> False

matchGlob :: Glob -> String -> Bool
matchGlob (Glob regex) val = 
	case execute regex val of
		Right (Just _) -> True
		_ -> False
