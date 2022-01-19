{- git-annex v9 -> v10 upgrade support
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Upgrade.V9 where

import Annex.Common
import Types.Upgrade
import Annex.Content
import Annex.Perms
import Git.ConfigTypes
import Types.RepoVersion
import Logs.Upgrade
import Utility.Daemon

import Data.Time.Clock.POSIX

upgrade :: Bool -> Annex UpgradeResult
upgrade automatic
	| automatic = do
		-- For automatic upgrade, wait until a year after the v9
		-- upgrade. This is to give time for any old processes
		-- that were running before the v9 upgrade to finish.
		-- Such old processes lock content using the old method,
		-- and it is not safe for such to still be running after
		-- this upgrade.
		timeOfUpgrade (RepoVersion 9) >>= \case
			Nothing -> performUpgrade automatic
			Just t -> do
				now <- liftIO getPOSIXTime
				if now - 365*24*60*60 > t
					then return UpgradeDeferred
					else checkassistantrunning $
						performUpgrade automatic
	| otherwise = performUpgrade automatic
  where
	-- Skip upgrade when git-annex assistant (or watch) is running,
	-- because these are long-running daemons that could conceivably
	-- run for an entire year.
	checkassistantrunning a = do
		pidfile <- fromRepo gitAnnexPidFile
		liftIO (checkDaemon (fromRawFilePath pidfile)) >>= \case
			Just _pid -> return UpgradeDeferred
			Nothing -> a

performUpgrade :: Bool -> Annex UpgradeResult
performUpgrade automatic = do
	unless automatic $
		showAction "v9 to v10"

	{- When core.sharedRepository is set, object files
	 - used to have their write bits set. That can now be removed,
	 - if the user the upgrade is running as has permission to remove
	 - it. (Otherwise, a later fsck will fix up the permissions.) -}
	withShared $ \sr -> case sr of
		GroupShared -> removewrite sr
		AllShared -> removewrite sr
		_ -> return ()

	return UpgradeDeferred
  where
	newver = Just (RepoVersion 9)

	removewrite sr = do
		ks <- listKeys InAnnex
		forM_ ks $ \k -> do
			obj <- calcRepo (gitAnnexLocation k)
			keystatus <- getKeyStatus k
			case keystatus of
				KeyPresent -> void $ tryIO $
					freezeContent'' sr obj newver
				KeyUnlockedThin -> return ()
				KeyLockedThin -> return ()
				KeyMissing -> return ()
