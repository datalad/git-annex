{- git-annex v4 -> v5 uppgrade support
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Upgrade.V4 where

import Annex.Common

{- Was only used for direct mode upgrade. v4 to v5 indirect update is a no-op,
 - and direct mode is no longer supported, so nothing needs to be done. -}
upgrade :: Bool -> Annex Bool
upgrade _automatic = return True
