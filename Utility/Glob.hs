{- file globbing
 -
 - This uses TDFA when available, with a fallback to regex-compat.
 - TDFA is less buggy in its support for non-unicode characters.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Glob (
	Glob,
	GlobCase(..),
	compileGlob,
	matchGlob
) where

import System.Path.WildMatch

#ifdef WITH_TDFA
import Text.Regex.TDFA
import Text.Regex.TDFA.String
#else
import Text.Regex
#endif

newtype Glob = Glob Regex

data GlobCase = CaseSensative | CaseInsensative

{- Compiles a glob to a regex, that can be repeatedly used. -}
compileGlob :: String -> GlobCase -> Glob
compileGlob glob globcase = Glob $
#ifdef WITH_TDFA
	case compile (defaultCompOpt {caseSensitive = casesentitive}) defaultExecOpt regex of
		Right r -> r
		Left _ -> error $ "failed to compile regex: " ++ regex
#else
	mkRegexWithOpts regex casesentitive True
#endif
  where
	regex = '^':wildToRegex glob
	casesentitive = case globcase of
		CaseSensative -> True
		CaseInsensative -> False

matchGlob :: Glob -> String -> Bool
matchGlob (Glob regex) val = 
#ifdef WITH_TDFA
	case execute regex val of
		Right (Just _) -> True
		_ -> False
#else
	isJust $ matchRegex regex val
#endif
