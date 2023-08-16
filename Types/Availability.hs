{- git-annex remote availability
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Availability where

data Availability = GloballyAvailable | LocallyAvailable | Unavailable
	deriving (Eq, Show)
