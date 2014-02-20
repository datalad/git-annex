{- git-annex file matcher types
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.FileMatcher where

import Types.Key (Key)

data MatchInfo
	= MatchingFile FileInfo
	| MatchingKey Key

data FileInfo = FileInfo
	{ relFile :: FilePath -- may be relative to cwd
	, matchFile :: FilePath -- filepath to match on; may be relative to top
	}
