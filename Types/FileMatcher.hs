{- git-annex file matcher types
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.FileMatcher where

import Types.UUID (UUID)
import Types.Key (Key)
import Utility.Matcher (Matcher, Token)

import qualified Data.Map as M
import qualified Data.Set as S

data MatchInfo
	= MatchingFile FileInfo
	| MatchingKey Key

data FileInfo = FileInfo
	{ relFile :: FilePath -- may be relative to cwd
	, matchFile :: FilePath -- filepath to match on; may be relative to top
	}

type FileMatcherMap a = M.Map UUID (Utility.Matcher.Matcher (S.Set UUID -> MatchInfo -> a Bool))

type MkLimit a = String -> Either String (MatchFiles a)

type AssumeNotPresent = S.Set UUID

type MatchFiles a = AssumeNotPresent -> MatchInfo -> a Bool

type FileMatcher a = Matcher (MatchFiles a)

-- This is a matcher that can have tokens added to it while it's being
-- built, and once complete is compiled to an unchangable matcher.
data ExpandableMatcher a
	= BuildingMatcher [Token (MatchInfo -> a Bool)]
	| CompleteMatcher (Matcher (MatchInfo -> a Bool))
