{- git-annex assistant mount watcher, using either dbus or mtab polling
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant.Threads.MountWatcher where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Utility.ThreadScheduler
import Utility.Mounts

import Control.Concurrent
import qualified Data.Set as S

#if WITH_DBUS
import DBus.Client
import DBus
#else
#warning Building without dbus support; will use mtab polling
#endif

mountWatcherThread :: ThreadState -> DaemonStatusHandle -> IO ()
mountWatcherThread st handle =
#if WITH_DBUS
	dbusThread st handle
#else
	pollingThread st handle
#endif

#if WITH_DBUS

dbusThread :: ThreadState -> DaemonStatusHandle -> IO ()
dbusThread st handle = (go =<< connectSession) `catchIO` onerr
	where
		go client = ifM (checkMountMonitor client)
			( do
				{- Store the current mount points in an mvar,
				 - to be compared later. We could in theory
				 - work out the mount point from the dbus
				 - message, but this is easier. -}
				mvar <- newMVar =<< currentMountPoints
				listen client mountAdded $ \_event -> do
					nowmounted <- currentMountPoints
					wasmounted <- swapMVar mvar nowmounted
					handleMounts st handle wasmounted nowmounted			
			, do
				runThreadState st $
					warning "No known volume monitor available through dbus; falling back to mtab polling"
				pollinstead
			)
		onerr e = do
			runThreadState st $
				warning $ "Failed to use dbus; falling back to mtab polling (" ++ show e ++ ")"
			pollinstead
		pollinstead = pollingThread st handle

listClientNames :: Client -> IO [String]
listClientNames client = do
	reply <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
		{ methodCallDestination = Just "org.freedesktop.DBus" }
	return $ fromMaybe [] $ fromVariant (methodReturnBody reply !! 0)

{- Examine the list of clients connected to dbus, to see if there
 - are any we can use to monitor mounts. -}
checkMountMonitor :: Client -> IO Bool
checkMountMonitor client = any (`elem` knownclients) <$> listClientNames client
	where
		knownclients = ["org.gtk.Private.GduVolumeMonitor"]

{- Filter matching events recieved when drives are mounted. -}	
mountAdded ::MatchRule
mountAdded = matchAny
	{ matchInterface = Just "org.gtk.Private.RemoteVolumeMonitor"
	, matchMember = Just "MountAdded"
	}

#endif

pollingThread :: ThreadState -> DaemonStatusHandle -> IO ()
pollingThread st handle = go =<< currentMountPoints
	where
		go wasmounted = do
			threadDelaySeconds (Seconds 10)
			nowmounted <- currentMountPoints
			handleMounts st handle wasmounted nowmounted
			go nowmounted

handleMounts :: ThreadState -> DaemonStatusHandle -> MountPoints -> MountPoints -> IO ()
handleMounts st handle wasmounted nowmounted = mapM_ (handleMount st handle) $
	S.toList $ newMountPoints wasmounted nowmounted

handleMount :: ThreadState -> DaemonStatusHandle -> Mntent -> IO ()
handleMount st handle mntent = do
	putStrLn $ "mounted: " ++ mnt_dir mntent

type MountPoints = S.Set Mntent

{- Reads mtab, getting the current set of mount points. -}
currentMountPoints :: IO MountPoints
currentMountPoints = S.fromList <$> getMounts

{- Finds new mount points, given an old and a new set. -}
newMountPoints :: MountPoints -> MountPoints -> MountPoints
newMountPoints old new = S.difference new old
