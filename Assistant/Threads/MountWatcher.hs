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
import Assistant.Threads.Pusher (pushToRemotes)
import Assistant.Alert
import qualified Annex
import qualified Git
import Utility.ThreadScheduler
import Utility.Mounts
import Remote.List
import qualified Types.Remote as Remote
import qualified Remote.Git
import qualified Command.Sync
import Assistant.Threads.Merger
import Logs.Remote

import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.Set as S
import Data.Time.Clock

#if WITH_DBUS
import DBus.Client
import DBus
import Data.Word (Word32)
#else
#warning Building without dbus support; will use mtab polling
#endif

thisThread :: ThreadName
thisThread = "MountWatcher"

mountWatcherThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> IO ()
mountWatcherThread st handle scanremotes =
#if WITH_DBUS
	dbusThread st handle scanremotes
#else
	pollingThread st handle scanremotes
#endif

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
				forM_ mountAdded $ \matcher ->
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

type ServiceName = String

listServiceNames :: Client -> IO [ServiceName]
listServiceNames client = do
	reply <- callDBus client "ListNames" []
	return $ fromMaybe [] $ fromVariant (methodReturnBody reply !! 0)

callDBus :: Client -> MemberName -> [Variant] -> IO MethodReturn
callDBus client name params = call_ client $
	(methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" name)
		{ methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallBody = params
		}

{- Examine the list of services connected to dbus, to see if there
 - are any we can use to monitor mounts. If not, will attempt to start one. -}
checkMountMonitor :: Client -> IO Bool
checkMountMonitor client = do
	running <- filter (`elem` usableservices)
		<$> listServiceNames client
	if null running
		then startOneService client startableservices
		else do
			debug thisThread [ "Using running DBUS service"
				, Prelude.head running
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

{- Filter matching events recieved when drives are mounted. -}	
mountAdded :: [MatchRule]
mountAdded = [gvfs, kde]
	where
		gvfs = matchAny
			{ matchInterface = Just "org.gtk.Private.RemoteVolumeMonitor"
			, matchMember = Just "MountAdded"
			}
		kde = matchAny
			{ matchInterface = Just "org.kde.Solid.Device"
			, matchMember = Just "setupDone"
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
handleMounts st dstatus scanremotes wasmounted nowmounted = mapM_ (handleMount st dstatus scanremotes) $
	S.toList $ newMountPoints wasmounted nowmounted

handleMount :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> Mntent -> IO ()
handleMount st dstatus scanremotes mntent = do
	debug thisThread ["detected mount of", dir]
	rs <- remotesUnder st dstatus mntent
	unless (null rs) $ do
		branch <- runThreadState st $ Command.Sync.currentBranch
		let nonspecial = filter (Git.repoIsLocal . Remote.repo) rs
		unless (null nonspecial) $
			void $ alertWhile dstatus (syncMountAlert dir nonspecial) $ do
				debug thisThread ["syncing with", show nonspecial]
				runThreadState st $ manualPull branch nonspecial
				now <- getCurrentTime	
				pushToRemotes thisThread now st Nothing nonspecial
		addScanRemotes scanremotes rs
	where
		dir = mnt_dir mntent

{- Finds remotes located underneath the mount point.
 -
 - Updates state to include the remotes.
 -
 - The config of git remotes is re-read, as it may not have been available
 - at startup time, or may have changed (it could even be a different
 - repository at the same remote location..)
 -}
remotesUnder :: ThreadState -> DaemonStatusHandle -> Mntent -> IO [Remote]
remotesUnder st dstatus mntent = runThreadState st $ do
	repotop <- fromRepo Git.repoPath
	rs <- remoteList
	pairs <- mapM (checkremote repotop) rs
	let (waschanged, rs') = unzip pairs
	when (any id waschanged) $ do
		Annex.changeState $ \s -> s { Annex.remotes = rs' }
		updateKnownRemotes dstatus
	return $ map snd $ filter fst pairs
	where
		checkremote repotop r = case Remote.path r of
			Just p | under mntent (absPathFrom repotop p) ->
				(,) <$> pure True <*> updateremote r
			_ -> return (False, r)
		updateremote r = do
			liftIO $ debug thisThread ["updating", show r]
			m <- readRemoteLog
			repo <- updaterepo $ Remote.repo r
			remoteGen m (Remote.remotetype r) repo
		updaterepo repo
			| Git.repoIsLocal repo || Git.repoIsLocalUnknown repo =
				Remote.Git.configRead repo
			| otherwise = return repo

type MountPoints = S.Set Mntent

currentMountPoints :: IO MountPoints
currentMountPoints = S.fromList <$> getMounts

newMountPoints :: MountPoints -> MountPoints -> MountPoints
newMountPoints old new = S.difference new old

{- Checks if a mount point contains a path. The path must be absolute. -}
under :: Mntent -> FilePath -> Bool
under = dirContains . mnt_dir
