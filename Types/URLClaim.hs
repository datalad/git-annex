{- git-annex url claiming
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.URLClaim where

data URLClaim = URLClaimed | URLClaimedAs FilePath
	deriving (Eq)
