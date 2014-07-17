{- git-annex assistant CredPair cache.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.CredPairCache where

import Types.Creds

import Control.Concurrent
import qualified Data.Map as M

type CredPairCache = MVar (M.Map Login Password)

newCredPairCache :: IO CredPairCache
newCredPairCache = newMVar M.empty
