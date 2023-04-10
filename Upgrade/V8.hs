{- git-annex v8 -> v9 upgrade support
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V8 where

import Annex.Common
import Types.Upgrade
import Config.Smudge

upgrade :: Bool -> Annex UpgradeResult
upgrade automatic = do
	unless automatic $
		showAction "v8 to v9"

	configureSmudgeFilterProcess

	return UpgradeSuccess
