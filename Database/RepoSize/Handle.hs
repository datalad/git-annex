{- Sqlite database used to track the sizes of repositories.
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Database.RepoSize.Handle where

import qualified Database.Handle as H
import Utility.LockPool (LockHandle)

import Control.Concurrent
import Data.Time.Clock.POSIX

data RepoSizeHandle = RepoSizeHandle
	(Maybe H.DbHandle)
	-- ^ Nothing if the database was not able to be opened due to
	-- permissions.
	(MVar (Maybe (LockHandle, POSIXTime)))
	-- ^ Live update lock and time of last check for stale live
	-- updates.
