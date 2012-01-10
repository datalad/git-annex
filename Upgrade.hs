{- git-annex upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade where

import Common.Annex
import Annex.Version
import qualified Upgrade.V0
import qualified Upgrade.V1
import qualified Upgrade.V2

upgrade :: Annex Bool
upgrade = go =<< getVersion
	where
		go (Just "0") = Upgrade.V0.upgrade
		go (Just "1") = Upgrade.V1.upgrade
		go (Just "2") = Upgrade.V2.upgrade
		go _ = return True
