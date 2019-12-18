{- git-annex upgrade support
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Upgrade where

import Annex.Common
import qualified Annex
import qualified Git
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
import qualified Upgrade.V7

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
		Just newv -> ifM (annexAutoUpgradeRepository <$> Annex.getGitConfig)
			( tryNonAsync (upgrade True newv) >>= \case
				Right True -> ok
				Right False -> err "Automatic upgrade failed!"
				Left ex -> err $ "Automatic upgrade exception! " ++ show ex
			, err "Automatic upgrade is disabled by annex.autoupgraderepository configuration. To upgrade this repository: git-annex upgrade"
			)
  where
	err msg = do
		g <- Annex.gitRepo
		p <- liftIO $ absPath $ fromRawFilePath $ Git.repoPath g
		return $ Just $ unwords
			[ "Repository", p
			, "is at unsupported version"
			, show (fromRepoVersion v) ++ "."
			, msg
			]
	ok = return Nothing

upgrade :: Bool -> RepoVersion -> Annex Bool
upgrade automatic destversion = do
	upgraded <- go =<< getVersion
	when upgraded $
		setVersion destversion
	return upgraded
  where
	go (Just v)
		| v >= destversion = return True
		| otherwise = ifM (up v)
			( go (Just (RepoVersion (fromRepoVersion v + 1)))
			, return False
			)
	go _ = return True

#ifndef mingw32_HOST_OS
	up (RepoVersion 0) = Upgrade.V0.upgrade
	up (RepoVersion 1) = Upgrade.V1.upgrade
#else
	up (RepoVersion 0) = giveup "upgrade from v0 on Windows not supported"
	up (RepoVersion 1) = giveup "upgrade from v1 on Windows not supported"
#endif
	up (RepoVersion 2) = Upgrade.V2.upgrade
	up (RepoVersion 3) = Upgrade.V3.upgrade automatic
	up (RepoVersion 4) = Upgrade.V4.upgrade automatic
	up (RepoVersion 5) = Upgrade.V5.upgrade automatic
	up (RepoVersion 6) = Upgrade.V6.upgrade automatic
	up (RepoVersion 7) = Upgrade.V7.upgrade automatic
	up _ = return True
