{- git-annex assistant network connection watcher, using dbus
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant.Threads.NetWatcher where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.Sync
import Utility.ThreadScheduler
import Remote.List
import qualified Types.Remote as Remote

#if WITH_DBUS
import Utility.DBus
import DBus.Client
import DBus
import Data.Word (Word32)
import qualified Control.Exception as E
#else
#warning Building without dbus support; will poll for network connection changes
#endif

thisThread :: ThreadName
thisThread = "NetWatcher"

netWatcherThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> NamedThread
#if WITH_DBUS
netWatcherThread st dstatus scanremotes = thread $
	dbusThread st dstatus scanremotes
#else
netWatcherThread _ _ _ = thread noop
#endif
	where
		thread = NamedThread thisThread

{- This is a fallback for when dbus cannot be used to detect
 - network connection changes, but it also ensures that
 - any networked remotes that may have not been routable for a
 - while (despite the local network staying up), are synced with
 - periodically. -}
netWatcherFallbackThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> NamedThread
netWatcherFallbackThread st dstatus scanremotes = thread $
	runEvery (Seconds 3600) $
		handleConnection st dstatus scanremotes
	where
		thread = NamedThread thisThread

#if WITH_DBUS

dbusThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> IO ()
dbusThread st dstatus scanremotes = E.catch (runClient getSystemAddress go) onerr
	where
		go client = ifM (checkNetMonitor client)
			( do
				listenNMConnections client handle
				listenWicdConnections client handle
			, do
				runThreadState st $
					warning "No known network monitor available through dbus; falling back to polling"
			)
		onerr :: E.SomeException -> IO ()
		onerr e = runThreadState st $
			warning $ "dbus failed; falling back to polling (" ++ show e ++ ")"
		handle = do
			debug thisThread ["detected network connection"]
			handleConnection st dstatus scanremotes

{- Examine the list of services connected to dbus, to see if there
 - are any we can use to monitor network connections. -}
checkNetMonitor :: Client -> IO Bool
checkNetMonitor client = do
	running <- filter (`elem` [networkmanager, wicd])
		<$> listServiceNames client
	case running of
		[] -> return False
		(service:_) -> do
			debug thisThread [ "Using running DBUS service"
				, service
				, "to monitor network connection events."
				]
			return True
	where
		networkmanager = "org.freedesktop.NetworkManager"
		wicd = "org.wicd.daemon"

{- Listens for new NetworkManager connections. -}
listenNMConnections :: Client -> IO () -> IO ()
listenNMConnections client callback =
	listen client matcher $ \event ->
		when (Just True == anyM activeconnection (signalBody event)) $
			callback
	where
		matcher = matchAny
			{ matchInterface = Just "org.freedesktop.NetworkManager.Connection.Active"
			, matchMember = Just "PropertiesChanged"
			}
		nm_connection_activated = toVariant (2 :: Word32)
		nm_state_key = toVariant ("State" :: String)
		activeconnection v = do
			m <- fromVariant v
			vstate <- lookup nm_state_key $ dictionaryItems m
			state <- fromVariant vstate
			return $ state == nm_connection_activated

{- Listens for new Wicd connections. -}
listenWicdConnections :: Client -> IO () -> IO ()
listenWicdConnections client callback =
	listen client matcher $ \event ->
		when (any (== wicd_success) (signalBody event)) $
			callback
	where
		matcher = matchAny
			{ matchInterface = Just "org.wicd.daemon"
			, matchMember = Just "ConnectResultsSent"
			}
		wicd_success = toVariant ("success" :: String)

#endif

handleConnection :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> IO ()
handleConnection st dstatus scanremotes =
	reconnectRemotes thisThread st dstatus scanremotes
		=<< networkRemotes st

{- Finds network remotes. -}
networkRemotes :: ThreadState -> IO [Remote]
networkRemotes st = runThreadState st $
	filter (isNothing . Remote.localpath) <$> remoteList
