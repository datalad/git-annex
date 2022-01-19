{- git-annex upgrade support
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Upgrade where

import Annex.Common
import Types.Upgrade
import qualified Annex
import qualified Git
import Config
import Annex.Path
import Annex.Version
import Types.RepoVersion
import Logs.Upgrade
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
import qualified Upgrade.V8
import qualified Upgrade.V9

import qualified Data.Map as M
import Data.Time.Clock.POSIX

checkUpgrade :: RepoVersion -> Annex ()
checkUpgrade = maybe noop giveup <=< needsUpgrade

needsUpgrade :: RepoVersion -> Annex (Maybe String)
needsUpgrade v
	| v `elem` supportedVersions = ok
	| otherwise = case M.lookup v autoUpgradeableVersions of
		Nothing
			| v `elem` upgradeableVersions ->
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
		p <- liftIO $ absPath $ Git.repoPath g
		return $ Just $ unwords
			[ "Repository", fromRawFilePath p
			, "is at unsupported version"
			, show (fromRepoVersion v) ++ "."
			, msg
			]
	ok = return Nothing

upgrade :: Bool -> RepoVersion -> Annex Bool
upgrade automatic destversion = do
	(upgraded, newversion) <- go =<< getVersion
	when upgraded $
		postupgrade newversion
	return upgraded
  where
	go (Just v)
		| v >= destversion = return (True, Just v)
		| otherwise = ifM upgradingRemote
			( upgraderemote
			, up v >>= \case
				UpgradeSuccess -> go (Just (incrversion v) )
				UpgradeFailed -> return (False, Just v)
				UpgradeDeferred -> return (True, Just v)
			)
	go Nothing = return (True, Nothing)

	incrversion v = RepoVersion (fromRepoVersion v + 1)

	postupgrade newversion = ifM upgradingRemote
		( reloadConfig
		, maybe noop upgradedto newversion
		)

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
	up (RepoVersion 8) = Upgrade.V8.upgrade automatic
	up (RepoVersion 9) = Upgrade.V9.upgrade automatic
	up _ = return UpgradeDeferred

	-- Upgrade local remotes by running git-annex upgrade in them.
	-- This avoids complicating the upgrade code by needing to handle
	-- upgrading a git repo other than the current repo.
	upgraderemote = do
		rp <- fromRawFilePath <$> fromRepo Git.repoPath
		gitAnnexChildProcess "upgrade"
			[ Param "--quiet"
			, Param "--autoonly"
			]
			(\p -> p { cwd = Just rp })
			(\_ _ _ pid -> waitForProcess pid >>= return . \case
				ExitSuccess -> (True, Nothing)
				_ -> (False, Nothing)
			)

	upgradedto v = do
		setVersion v
		writeUpgradeLog v =<< liftIO getPOSIXTime

upgradingRemote :: Annex Bool
upgradingRemote = isJust <$> fromRepo Git.remoteName
