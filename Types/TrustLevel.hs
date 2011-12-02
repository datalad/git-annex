{- git-annex trust levels
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.TrustLevel (
	TrustLevel(..),
	TrustMap
) where

import qualified Data.Map as M

import Types.UUID

data TrustLevel = Trusted | SemiTrusted | UnTrusted | DeadTrusted
	deriving Eq

type TrustMap = M.Map UUID TrustLevel
