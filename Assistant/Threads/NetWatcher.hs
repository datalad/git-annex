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
import Assistant.Sync
import Utility.ThreadScheduler
import qualified Types.Remote as Remote
import Assistant.DaemonStatus

#if WITH_DBUS
import Utility.DBus
import DBus.Client
import DBus
import Data.Word (Word32)
import Assistant.NetMessager
#else
#warning Building without dbus support; will poll for network connection changes
#endif

netWatcherThread :: NamedThread
#if WITH_DBUS
netWatcherThread = thread dbusThread
#else
netWatcherThread = thread noop
#endif
  where
	thread = namedThread "NetWatcher"

{- This is a fallback for when dbus cannot be used to detect
 - network connection changes, but it also ensures that
 - any networked remotes that may have not been routable for a
 - while (despite the local network staying up), are synced with
 - periodically.
 -
 - Note that it does not call notifyNetMessagerRestart, because
 - it doesn't know that the network has changed.
 -}
netWatcherFallbackThread :: NamedThread
netWatcherFallbackThread = namedThread "NetWatcherFallback" $
	runEvery (Seconds 3600) <~> handleConnection

#if WITH_DBUS

dbusThread :: Assistant ()
dbusThread = do
	handleerr <- asIO2 onerr
	runclient <- asIO1 go
	liftIO $ persistentClient getSystemAddress () handleerr runclient
  where
	go client = ifM (checkNetMonitor client)
		( do
			listenNMConnections client <~> handleconn
			listenWicdConnections client <~> handleconn
		, do
			liftAnnex $
				warning "No known network monitor available through dbus; falling back to polling"
		)
	handleconn = do
		debug ["detected network connection"]
		notifyNetMessagerRestart
		handleConnection
	onerr e _ = do
		liftAnnex $
			warning $ "lost dbus connection; falling back to polling (" ++ show e ++ ")"
		{- Wait, in hope that dbus will come back -}
		liftIO $ threadDelaySeconds (Seconds 60)

{- Examine the list of services connected to dbus, to see if there
 - are any we can use to monitor network connections. -}
checkNetMonitor :: Client -> Assistant Bool
checkNetMonitor client = do
	running <- liftIO $ filter (`elem` [networkmanager, wicd])
		<$> listServiceNames client
	case running of
		[] -> return False
		(service:_) -> do
			debug [ "Using running DBUS service"
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

handleConnection :: Assistant ()
handleConnection = reconnectRemotes True =<< networkRemotes

{- Network remotes to sync with. -}
networkRemotes :: Assistant [Remote]
networkRemotes = filter (isNothing . Remote.localpath) . syncRemotes
	<$> getDaemonStatus
