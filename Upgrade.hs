{- git-annex upgrade support
 -
 - Copyright 2010, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Upgrade where

import Annex.Common
import Annex.Version
#ifndef mingw32_HOST_OS
import qualified Upgrade.V0
import qualified Upgrade.V1
#endif
import qualified Upgrade.V2
import qualified Upgrade.V3
import qualified Upgrade.V4
import qualified Upgrade.V5

checkUpgrade :: Version -> Annex ()
checkUpgrade = maybe noop error <=< needsUpgrade

needsUpgrade :: Version -> Annex (Maybe String)
needsUpgrade v
	| v `elem` supportedVersions = ok
	| v `elem` autoUpgradeableVersions = ifM (upgrade True)
		( ok
		, err "Automatic upgrade failed!"
		)
	| v `elem` upgradableVersions = err "Upgrade this repository: git-annex upgrade"
	| otherwise = err "Upgrade git-annex."
  where
	err msg = return $ Just $ "Repository version " ++ v ++
		" is not supported. " ++ msg
	ok = return Nothing

upgrade :: Bool -> Annex Bool
upgrade automatic = do
	upgraded <- go =<< getVersion
	when upgraded $
		setVersion latestVersion
	return upgraded
  where
#ifndef mingw32_HOST_OS
	go (Just "0") = Upgrade.V0.upgrade
	go (Just "1") = Upgrade.V1.upgrade
#else
	go (Just "0") = error "upgrade from v0 on Windows not supported"
	go (Just "1") = error "upgrade from v1 on Windows not supported"
#endif
	go (Just "2") = Upgrade.V2.upgrade
	go (Just "3") = Upgrade.V3.upgrade automatic
	go (Just "4") = Upgrade.V4.upgrade automatic
	go (Just "5") = Upgrade.V5.upgrade automatic
	go _ = return True
