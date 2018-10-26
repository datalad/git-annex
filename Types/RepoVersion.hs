{- git-annex repository versioning
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.RepoVersion where

newtype RepoVersion = RepoVersion { fromRepoVersion :: Int }
	deriving (Eq, Ord, Read, Show)
