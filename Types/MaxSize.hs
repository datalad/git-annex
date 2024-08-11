{- git-annex maxsize type
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.MaxSize where

newtype MaxSize = MaxSize Integer
	deriving (Show, Eq, Ord)
