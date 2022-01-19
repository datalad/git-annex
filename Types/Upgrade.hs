{- git-annex upgrade types
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Upgrade where

data UpgradeResult = UpgradeSuccess | UpgradeFailed | UpgradeDeferred
