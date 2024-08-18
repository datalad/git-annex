{- git-annex repo sizes types
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.RepoSize where

-- The current size of a repo.
newtype RepoSize = RepoSize { fromRepoSize :: Integer }
	deriving (Show, Eq, Ord, Num)

-- The maximum size of a repo.
newtype MaxSize = MaxSize { fromMaxSize :: Integer }
	deriving (Show, Eq, Ord)
