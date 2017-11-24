{- git-annex remote availability
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Availability where

data Availability = GloballyAvailable | LocallyAvailable
	deriving (Eq, Show, Read)
