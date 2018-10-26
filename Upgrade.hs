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
import Types.RepoVersion
#ifndef mingw32_HOST_OS
import qualified Upgrade.V0
import qualified Upgrade.V1
#endif
import qualified Upgrade.V2
import qualified Upgrade.V3
import qualified Upgrade.V4
import qualified Upgrade.V5
import qualified Upgrade.V6

import qualified Data.Map as M

checkUpgrade :: RepoVersion -> Annex ()
checkUpgrade = maybe noop giveup <=< needsUpgrade

needsUpgrade :: RepoVersion -> Annex (Maybe String)
needsUpgrade v
	| v `elem` supportedVersions = ok
	| otherwise = case M.lookup v autoUpgradeableVersions of
		Nothing
			| v `elem` upgradableVersions ->
				err "Upgrade this repository: git-annex upgrade"
			| otherwise ->
				err "Upgrade git-annex."
		Just newv -> ifM (upgrade True newv)
			( ok
			, err "Automatic upgrade failed!"
			)
  where
	err msg = return $ Just $ "Repository version " ++
		show (fromRepoVersion v) ++
		" is not supported. " ++ msg
	ok = return Nothing

upgrade :: Bool -> RepoVersion -> Annex Bool
upgrade automatic destversion = do
	upgraded <- go =<< getVersion
	when upgraded $
		setVersion destversion
	return upgraded
  where
	go (Just v) | v >= destversion = return True
#ifndef mingw32_HOST_OS
	go (Just (RepoVersion 0)) = Upgrade.V0.upgrade
	go (Just (RepoVersion 1)) = Upgrade.V1.upgrade
#else
	go (Just (RepoVersion 0)) = giveup "upgrade from V0 on Windows not supported"
	go (Just (RepoVersion 1)) = giveup "upgrade from V1 on Windows not supported"
#endif
	go (Just (RepoVersion 2)) = Upgrade.V2.upgrade
	go (Just (RepoVersion 3)) = Upgrade.V3.upgrade automatic
	go (Just (RepoVersion 4)) = Upgrade.V4.upgrade automatic
	go (Just (RepoVersion 5)) = Upgrade.V5.upgrade automatic
	go (Just (RepoVersion 6)) = Upgrade.V6.upgrade automatic
	go _ = return True
