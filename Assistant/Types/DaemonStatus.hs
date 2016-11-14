{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.DaemonStatus where

import Annex.Common
import Assistant.Pairing
import Utility.NotificationBroadcaster
import Types.Transfer
import Assistant.Types.ThreadName
import Assistant.Types.Alert
import Utility.Url

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Data.Time.Clock.POSIX
import qualified Data.Map as M
import qualified Data.Set as S

data DaemonStatus = DaemonStatus
	-- All the named threads that comprise the daemon,
	-- and actions to run to restart them.
	{ startedThreads :: M.Map ThreadName (Async (), IO ())
	-- False when the daemon is performing its startup scan
	, scanComplete :: Bool
	-- True when all files should be restaged.
	, forceRestage :: Bool
	-- Time when a previous process of the daemon was running ok
	, lastRunning :: Maybe POSIXTime
	-- True when the daily sanity checker is running
	, sanityCheckRunning :: Bool
	-- Last time the daily sanity checker ran
	, lastSanityCheck :: Maybe POSIXTime
	-- True when a scan for file transfers is running
	, transferScanRunning :: Bool
	-- Currently running file content transfers
	, currentTransfers :: TransferMap
	-- Messages to display to the user.
	, alertMap :: AlertMap
	, lastAlertId :: AlertId
	-- Ordered list of all remotes that can be synced with
	, syncRemotes :: [Remote]
	-- Ordered list of remotes to sync git with
	, syncGitRemotes :: [Remote]
	-- Ordered list of remotes to sync data with
	, syncDataRemotes :: [Remote]
	-- Are we syncing to any cloud remotes?
	, syncingToCloudRemote :: Bool
	-- Set of uuids of remotes that are currently connected.
	, currentlyConnectedRemotes :: S.Set UUID
	-- Pairing request that is in progress.
	, pairingInProgress :: Maybe PairingInProgress
	-- Broadcasts notifications about all changes to the DaemonStatus.
	, changeNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when queued or current transfers change.
	, transferNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when there's a change to the alerts.
	, alertNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when the syncRemotes change.
	, syncRemotesNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when the scheduleLog changes.
	, scheduleLogNotifier :: NotificationBroadcaster
	-- Broadcasts a notification once the startup sanity check has run.
	, startupSanityCheckNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when the network is connected.
	, networkConnectedNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when a global redirect is needed.
	, globalRedirNotifier :: NotificationBroadcaster
	, globalRedirUrl :: Maybe URLString
	-- Actions to run after a Key is transferred.
	, transferHook :: M.Map Key (Transfer -> IO ())
	-- MVars to signal when a remote gets connected.
	, connectRemoteNotifiers :: M.Map UUID [MVar ()]
	}

type TransferMap = M.Map Transfer TransferInfo

type DaemonStatusHandle = TVar DaemonStatus

newDaemonStatus :: IO DaemonStatus
newDaemonStatus = DaemonStatus
	<$> pure M.empty
	<*> pure False
	<*> pure False
	<*> pure Nothing
	<*> pure False
	<*> pure Nothing
	<*> pure False
	<*> pure M.empty
	<*> pure M.empty
	<*> pure firstAlertId
	<*> pure []
	<*> pure []
	<*> pure []
	<*> pure False
	<*> pure S.empty
	<*> pure Nothing
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> pure Nothing
	<*> pure M.empty
	<*> pure M.empty
