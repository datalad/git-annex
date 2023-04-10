{- git-annex v6 -> v7 upgrade support
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V6 where

import Annex.Common
import Types.Upgrade
import Config
import Annex.Hook

upgrade :: Bool -> Annex UpgradeResult
upgrade automatic = do
	unless automatic $
		showAction "v6 to v7"
	unlessM isBareRepo $ do
		hookWrite postCheckoutHook
		hookWrite postMergeHook
	return UpgradeSuccess
