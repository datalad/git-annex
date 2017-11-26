{- git-annex v4 -> v5 uppgrade support
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V4 where

import Annex.Common
import Config
import Annex.Direct

{- Direct mode only upgrade. v4 to v5 indirect update is a no-op -}
upgrade :: Bool -> Annex Bool
upgrade automatic = ifM isDirect
	( do
		unless automatic $
			showAction "v4 to v5"
		setDirect True
		return True
	, return True
	)
