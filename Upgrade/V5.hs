{- git-annex v5 -> v6 uppgrade support
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V5 where

import Common.Annex
import Config
import Annex.InodeSentinal

upgrade :: Bool -> Annex Bool
upgrade automatic = do
	unless automatic $
		showAction "v5 to v6"
	configureSmudgeFilter
	-- Inode sentinal file was only used in direct mode and when
	-- locking down files as they were added. In v6, it's used more
	-- extensively, so make sure it exists, since old repos that didn't
	-- use direct mode may not have created it.
	unlessM (isDirect) $
		createInodeSentinalFile True
	return True
