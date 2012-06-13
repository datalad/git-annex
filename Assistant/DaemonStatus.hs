{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.DaemonStatus where

import Common.Annex

import Control.Concurrent
import System.Posix.Types

data DaemonStatus = DaemonStatus
	-- False when the daemon is performing its startup scan
	{ scanComplete :: Bool
	-- Time when a previous process of the daemon was running ok
	, lastRunning :: Maybe EpochTime
	}

type DaemonStatusHandle = MVar DaemonStatus

newDaemonStatus :: DaemonStatus
newDaemonStatus = DaemonStatus
	{ scanComplete = False
	, lastRunning = Nothing
	}

startDaemonStatus :: IO DaemonStatusHandle
startDaemonStatus = newMVar newDaemonStatus

getDaemonStatus :: DaemonStatusHandle -> Annex DaemonStatus
getDaemonStatus = liftIO . readMVar

modifyDaemonStatus :: DaemonStatusHandle -> (DaemonStatus -> DaemonStatus) -> Annex ()
modifyDaemonStatus status a = liftIO $ modifyMVar_ status (return . a)
