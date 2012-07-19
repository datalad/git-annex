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
dbusThread st handle = do
	r <- tryIO connectSession
	case r of
		Left e -> do
			print $ "Failed to connect to dbus; falling back to mtab polling (" ++ show e ++ ")"
			pollingThread st handle
		Right client -> do
			{- Store the current mount points in an mvar,
			 - to be compared later. We could in theory work
			 - out the mount point from the dbus message, but
			 - this is easier. -}
			mvar <- newMVar =<< currentMountPoints
			-- Spawn a listener thread, and returns.
			listen client mountadded (go mvar)
	where
		mountadded = matchAny
			{ matchInterface = Just "org.gtk.Private.RemoteVolumeMonitor"
			, matchMember = Just "MountAdded"
			}
		go mvar event = do
			nowmounted <- currentMountPoints
			wasmounted <- swapMVar mvar nowmounted
			handleMounts st handle wasmounted nowmounted

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

handleMount :: ThreadState -> DaemonStatusHandle -> FilePath -> IO ()
handleMount st handle mountpoint = do
	putStrLn $ "mounted: " ++ mountpoint

type MountPoints = S.Set FilePath

{- Reads mtab, getting the current set of mount points. -}
currentMountPoints :: IO MountPoints
currentMountPoints = S.fromList . map mnt_dir <$> read_mtab

{- Finds new mount points, given an old and a new set. -}
newMountPoints :: MountPoints -> MountPoints -> MountPoints
newMountPoints old new = S.difference new old
