{- git-annex v5 -> v6 uppgrade support
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V5 where

import Common.Annex
import Config

upgrade :: Bool -> Annex Bool
upgrade automatic = do
	unless automatic $
		showAction "v5 to v6"
	configureSmudgeFilter
	return True
