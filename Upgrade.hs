{- git-annex upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Upgrade where

import Common.Annex
import Annex.Version
#ifndef mingw32_HOST_OS
import qualified Upgrade.V0
import qualified Upgrade.V1
#endif
import qualified Upgrade.V2
import qualified Upgrade.V4

checkUpgrade :: Version -> Annex ()
checkUpgrade v
	| v `elem` supportedVersions = noop
	| v `elem` autoUpgradeableVersions = unlessM (upgrade True) $
		err "Automatic upgrade failed!"
	| v `elem` upgradableVersions = err "Upgrade this repository: git-annex upgrade"
	| otherwise = err "Upgrade git-annex."
  where
	err msg = error $ "Repository version " ++ v ++
		" is not supported. " ++ msg

upgrade :: Bool -> Annex Bool
upgrade automatic = go =<< getVersion
  where
#ifndef mingw32_HOST_OS
	go (Just "0") = Upgrade.V0.upgrade
	go (Just "1") = Upgrade.V1.upgrade
#else
	go (Just "0") = error "upgrade from v0 on Windows not supported"
	go (Just "1") = error "upgrade from v1 on Windows not supported"
#endif
	go (Just "2") = Upgrade.V2.upgrade
	go (Just "4") = Upgrade.V4.upgrade automatic
	go _ = return True
