{- git-annex upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade where

import Types
import Version
import qualified Upgrade.V0
import qualified Upgrade.V1
import qualified Upgrade.V2

{- Uses the annex.version git config setting to automate upgrades. -}
upgrade :: Annex Bool
upgrade = do
	version <- getVersion
	case version of
		Just "0" -> Upgrade.V0.upgrade
		Just "1" -> Upgrade.V1.upgrade
		Just "2" -> Upgrade.V2.upgrade
		_ -> return True
