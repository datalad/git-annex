{- git-annex assistant mount watcher, using either dbus or mtab polling
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant.Threads.MountWatcher where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.Sync
import qualified Annex
import qualified Git
import Utility.ThreadScheduler
import Utility.Mounts
import Remote.List
import qualified Types.Remote as Remote

import qualified Data.Set as S

#if WITH_DBUS
import Utility.DBus
import DBus.Client
import DBus
import Data.Word (Word32)
import Control.Concurrent
import qualified Control.Exception as E
#else
#warning Building without dbus support; will use mtab polling
#endif

thisThread :: ThreadName
thisThread = "MountWatcher"

mountWatcherThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> NamedThread
mountWatcherThread st handle scanremotes = thread $
#if WITH_DBUS
	dbusThread st handle scanremotes
#else
	pollingThread st handle scanremotes
#endif
	where
		thread = NamedThread thisThread

#if WITH_DBUS

dbusThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> IO ()
dbusThread st dstatus scanremotes = E.catch (go =<< connectSession) onerr
	where
		go client = ifM (checkMountMonitor client)
			( do
				{- Store the current mount points in an mvar,
				 - to be compared later. We could in theory
				 - work out the mount point from the dbus
				 - message, but this is easier. -}
				mvar <- newMVar =<< currentMountPoints
				forM_ mountChanged $ \matcher ->
					listen client matcher $ \_event -> do
						nowmounted <- currentMountPoints
						wasmounted <- swapMVar mvar nowmounted
						handleMounts st dstatus scanremotes wasmounted nowmounted
			, do
				runThreadState st $
					warning "No known volume monitor available through dbus; falling back to mtab polling"
				pollinstead
			)
		onerr :: E.SomeException -> IO ()
		onerr e = do
			runThreadState st $
				warning $ "Failed to use dbus; falling back to mtab polling (" ++ show e ++ ")"
			pollinstead
		pollinstead = pollingThread st dstatus scanremotes

{- Examine the list of services connected to dbus, to see if there
 - are any we can use to monitor mounts. If not, will attempt to start one. -}
checkMountMonitor :: Client -> IO Bool
checkMountMonitor client = do
	running <- filter (`elem` usableservices)
		<$> listServiceNames client
	case running of
		[] -> startOneService client startableservices
		(service:_) -> do
			debug thisThread [ "Using running DBUS service"
				, service
				, "to monitor mount events."
				]
			return True
	where
		startableservices = [gvfs]
		usableservices = startableservices ++ [kde]
		gvfs = "org.gtk.Private.GduVolumeMonitor"
		kde = "org.kde.DeviceNotifications"

startOneService :: Client -> [ServiceName] -> IO Bool
startOneService _ [] = return False
startOneService client (x:xs) = do
	_ <- callDBus client "StartServiceByName"
		[toVariant x, toVariant (0 :: Word32)]
	ifM (elem x <$> listServiceNames client)
		( do
			debug thisThread [ "Started DBUS service"
				, x
				, "to monitor mount events."
				]
			return True
		, startOneService client xs
		)

{- Filter matching events recieved when drives are mounted and unmounted. -}	
mountChanged :: [MatchRule]
mountChanged = [gvfs True, gvfs False, kde, kdefallback]
	where
		{- gvfs reliably generates this event whenever a drive is mounted/unmounted,
		 - whether automatically, or manually -}
		gvfs mount = matchAny
			{ matchInterface = Just "org.gtk.Private.RemoteVolumeMonitor"
			, matchMember = Just $ if mount then "MountAdded" else "MountRemoved"
			}
		{- This event fires when KDE prompts the user what to do with a drive,
		 - but maybe not at other times. And it's not received -}
		kde = matchAny
			{ matchInterface = Just "org.kde.Solid.Device"
			, matchMember = Just "setupDone"
			}
		{- This event may not be closely related to mounting a drive, but it's
		 - observed reliably when a drive gets mounted or unmounted. -}
		kdefallback = matchAny
			{ matchInterface = Just "org.kde.KDirNotify"
			, matchMember = Just "enteredDirectory"
			}

#endif

pollingThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> IO ()
pollingThread st dstatus scanremotes = go =<< currentMountPoints
	where
		go wasmounted = do
			threadDelaySeconds (Seconds 10)
			nowmounted <- currentMountPoints
			handleMounts st dstatus scanremotes wasmounted nowmounted
			go nowmounted

handleMounts :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> MountPoints -> MountPoints -> IO ()
handleMounts st dstatus scanremotes wasmounted nowmounted =
	mapM_ (handleMount st dstatus scanremotes . mnt_dir) $
		S.toList $ newMountPoints wasmounted nowmounted

handleMount :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> FilePath -> IO ()
handleMount st dstatus scanremotes dir = do
	debug thisThread ["detected mount of", dir]
	reconnectRemotes thisThread st dstatus scanremotes
		=<< filter (Git.repoIsLocal . Remote.repo)
			<$> remotesUnder st dstatus dir

{- Finds remotes located underneath the mount point.
 -
 - Updates state to include the remotes.
 -
 - The config of git remotes is re-read, as it may not have been available
 - at startup time, or may have changed (it could even be a different
 - repository at the same remote location..)
 -}
remotesUnder :: ThreadState -> DaemonStatusHandle -> FilePath -> IO [Remote]
remotesUnder st dstatus dir = runThreadState st $ do
	repotop <- fromRepo Git.repoPath
	rs <- remoteList
	pairs <- mapM (checkremote repotop) rs
	let (waschanged, rs') = unzip pairs
	when (any id waschanged) $ do
		Annex.changeState $ \s -> s { Annex.remotes = rs' }
		updateSyncRemotes dstatus
	return $ map snd $ filter fst pairs
	where
		checkremote repotop r = case Remote.localpath r of
			Just p | dirContains dir (absPathFrom repotop p) ->
				(,) <$> pure True <*> updateRemote r
			_ -> return (False, r)

type MountPoints = S.Set Mntent

currentMountPoints :: IO MountPoints
currentMountPoints = S.fromList <$> getMounts

newMountPoints :: MountPoints -> MountPoints -> MountPoints
newMountPoints old new = S.difference new old
