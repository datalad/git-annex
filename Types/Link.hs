{- Types for links to content
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Link where

-- A locked link is stored in git as a symlink, while an unlocked link is
-- stored as a pointer file.
data LinkType = LockedLink | UnlockedLink
	deriving (Show, Eq)
