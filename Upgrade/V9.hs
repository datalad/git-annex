{- git-annex v9 -> v10 upgrade support
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V9 where

import Annex.Common
import qualified Annex
import Types.Upgrade
import Annex.Content
import Annex.Perms
import Annex.LockFile
import Annex.Version
import Git.ConfigTypes
import Types.RepoVersion
import Logs.Upgrade
import Utility.Daemon

import Data.Time.Clock.POSIX

upgrade :: Bool -> Annex UpgradeResult
upgrade automatic
	| automatic = ifM oldprocessesdanger
		( return UpgradeDeferred
		, performUpgrade automatic
		)
	| otherwise = ifM (oldprocessesdanger <&&> (not <$> Annex.getRead Annex.force))
		( do
			warning $ UnquotedString $ unlines unsafeupgrade
			return UpgradeDeferred
		, performUpgrade automatic
		)
  where
	{- Wait until a year after the v9 upgrade, to give time for
	 - any old processes that were running before the v9 upgrade
	 - to finish. Such old processes lock content using the old method,
	 - and it is not safe for such to still be running after
	 - this upgrade. -}
	oldprocessesdanger = timeOfUpgrade (RepoVersion 9) >>= \case
		Just t -> do
			now <- liftIO getPOSIXTime
			if now < t + 365*24*60*60
				then return True
				else assistantrunning
		-- Initialized at v9, so no old process danger exists.
		Nothing -> pure False

	{- Skip upgrade when git-annex assistant (or watch) is running,
	 - because these are long-running daemons that could conceivably
	 - run for an entire year and so predate the v9 upgrade. -}
	assistantrunning = do
		pidfile <- fromRepo gitAnnexPidFile
		isJust <$> liftIO (checkDaemon (fromOsPath pidfile))
	
	unsafeupgrade =
		[ "Not upgrading from v9 to v10, because there may be git-annex"
		, "processes running that predate the v9 upgrade. Upgrading with"
		, "such processes running could lead to data loss. This upgrade"
		, "will be deferred until one year after the v9 upgrade to make"
		, "sure there are no such old processes running."
		, "(Use --force to upgrade immediately.)"
		]

performUpgrade :: Bool -> Annex UpgradeResult
performUpgrade automatic = do
	unless automatic $
		showAction "v9 to v10"
	
	{- Take a lock to ensure that there are no other git-annex
	 - processes running that are using the old content locking method. -}
	lck <- fromRepo gitAnnexContentLockLock
	withExclusiveLock lck $ do
		{- When core.sharedRepository is set, object files
		 - used to have their write bits set. That can now be
		 - removed, if the user the upgrade is running as has
		 - permission to remove it.
		 - (Otherwise, a later fsck will fix up the permissions.) -}
		withShared $ \sr -> case sr of
			GroupShared -> removewrite sr
			AllShared -> removewrite sr
			_ -> return ()

		{- Set the new version while still holding the lock,
		 - so that any other process waiting for the lock will
		 - be able to detect that the upgrade happened. -}
		setVersion newver

		return UpgradeSuccess
  where
	newver = RepoVersion 10

	removewrite sr = do
		ks <- listKeys InAnnex
		forM_ ks $ \k -> do
			obj <- calcRepo (gitAnnexLocation k)
			keystatus <- getKeyStatus k
			case keystatus of
				KeyPresent -> void $ tryIO $
					freezeContent'' sr obj (Just newver)
				KeyUnlockedThin -> return ()
				KeyLockedThin -> return ()
				KeyMissing -> return ()
