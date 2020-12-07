{- A pool of "git-annex transferkeys" processes available for use
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Types.TransferrerPool (
	module Types.TransferrerPool,
	checkNetworkConnections,
) where

import Types.TransferrerPool
import Utility.NotificationBroadcaster
import Assistant.Types.DaemonStatus

import Control.Concurrent.STM

checkNetworkConnections :: DaemonStatusHandle -> MkCheckTransferrer
checkNetworkConnections dstatushandle = do
	dstatus <- atomically $ readTVar dstatushandle
	h <- newNotificationHandle False (networkConnectedNotifier dstatus)
	return $ not <$> checkNotification h
