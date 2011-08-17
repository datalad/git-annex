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

data TrustLevel = SemiTrusted | UnTrusted | Trusted
	deriving Eq

instance Show TrustLevel where
        show SemiTrusted = "?"
        show UnTrusted = "0"
        show Trusted = "1"

instance Read TrustLevel where
        readsPrec _ "1" = [(Trusted, "")]
        readsPrec _ "0" = [(UnTrusted, "")]
	readsPrec _ _ = [(SemiTrusted, "")]

type TrustMap = M.Map UUID TrustLevel
