{- git-annex v8 -> v9 upgrade support
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Upgrade.V8 where

import Annex.Common
import Types.Upgrade
import Utility.Daemon

upgrade :: Bool -> Annex UpgradeResult
upgrade automatic = do
	-- Skip running when git-annex assistant (or watch) is running,
	-- because these are long-running daemons that could conceivably
	-- run for an entire year, and so still be running when the v10
	-- upgrade happens. If the assistant then tried to drop a file
	-- after the v10 upgrade, it would use the wrong content file
	-- locking, which could lead to data loss. The remotedaemon does
	-- not drop content, so will not block the upgrade.
	pidfile <- fromRepo gitAnnexPidFile
	liftIO (checkDaemon (fromRawFilePath pidfile)) >>= \case
		Just _pid
			| automatic -> return UpgradeDeferred
			| otherwise -> giveup "Cannot upgrade to v9 when git-annex assistant or watch daemon is running."
		Nothing -> do
			unless automatic $
				showAction "v8 to v9"

			return UpgradeSuccess
