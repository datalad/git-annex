{- git-annex assistant network connection watcher, using dbus
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
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
import Utility.NotificationBroadcaster

#if WITH_DBUS
import Assistant.RemoteControl
import Utility.DBus
import DBus.Client
import DBus
#else
#ifdef linux_HOST_OS
#warning Building without dbus support; will poll for network connection changes
#endif
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
 - Note that it does not signal the RemoteControl, because it doesn't 
 - know that the network has changed.
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
			callback <- asIO1 connchange
			liftIO $ do
				listenNMConnections client callback
				listenNDConnections client callback
				listenWicdConnections client callback
		, do
			liftAnnex $
				warning "No known network monitor available through dbus; falling back to polling"
		)
	connchange False = do
		debug ["detected network disconnection"]
		sendRemoteControl LOSTNET
	connchange True = do
		debug ["detected network connection"]
		handleConnection
		sendRemoteControl RESUME
	onerr e _ = do
		liftAnnex $
			warning $ "lost dbus connection; falling back to polling (" ++ show e ++ ")"
		{- Wait, in hope that dbus will come back -}
		liftIO $ threadDelaySeconds (Seconds 60)

{- Examine the list of services connected to dbus, to see if there
 - are any we can use to monitor network connections. -}
checkNetMonitor :: Client -> Assistant Bool
checkNetMonitor client = do
	running <- liftIO $ filter (`elem` manager_addresses)
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
	manager_addresses = [networkmanager, networkd, wicd]
	networkmanager = "org.freedesktop.NetworkManager"
	networkd = "org.freedesktop.network1"
	wicd = "org.wicd.daemon"

{- Listens for systemd-networkd connections and diconnections.
 -
 - Connection example (once fully connected):
 - [Variant {"OperationalState": Variant "routable"}]
 -
 - Disconnection example:
 - [Variant {"OperationalState": Variant _}]
 -}
listenNDConnections :: Client -> (Bool -> IO ()) -> IO ()
listenNDConnections client setconnected =
	void $ addMatch client matcher
		$ \event -> mapM_ handleevent
			(map dictionaryItems $ mapMaybe fromVariant $ signalBody event)
  where
	matcher = matchAny
		{ matchInterface = Just "org.freedesktop.DBus.Properties"
		, matchMember = Just "PropertiesChanged"
		}
	operational_state_key = toVariant ("OperationalState" :: String)
	routable = toVariant $ toVariant ("routable" :: String)
	handleevent m = case lookup operational_state_key m of
				Just state -> if state == routable
					then setconnected True
					else setconnected False
				Nothing -> noop

{- Listens for NetworkManager connections and diconnections.
 -
 - Connection example (once fully connected):
 - [Variant {"ActivatingConnection": Variant (ObjectPath "/"), "PrimaryConnection": Variant (ObjectPath "/org/freedesktop/NetworkManager/ActiveConnection/34"), "State": Variant 70}]
 -
 - Disconnection example:
 - [Variant {"ActiveConnections": Variant []}]
 -}
listenNMConnections :: Client -> (Bool -> IO ()) -> IO ()
listenNMConnections client setconnected =
	void $ addMatch client matcher
		$ \event -> mapM_ handleevent
			(map dictionaryItems $ mapMaybe fromVariant $ signalBody event)
  where
	matcher = matchAny
		{ matchInterface = Just "org.freedesktop.NetworkManager"
		, matchMember = Just "PropertiesChanged"
		}
	nm_active_connections_key = toVariant ("ActiveConnections" :: String)
	nm_activatingconnection_key = toVariant ("ActivatingConnection" :: String)
	noconnections = Just $ toVariant $ toVariant ([] :: [ObjectPath])
	rootconnection = Just $ toVariant $ toVariant $ objectPath_ "/"
	handleevent m
		| lookup nm_active_connections_key m == noconnections =
			setconnected False
		| lookup nm_activatingconnection_key m == rootconnection =
			setconnected True
		| otherwise = noop

{- Listens for Wicd connections and disconnections.
 -
 - Connection example:
 -   ConnectResultsSent:
 -     Variant "success"
 -
 - Diconnection example:
 -   StatusChanged
 -     [Variant 0, Variant [Varient ""]]
 -}
listenWicdConnections :: Client -> (Bool -> IO ()) -> IO ()
listenWicdConnections client setconnected = do
	match connmatcher $ \event ->
		when (any (== wicd_success) (signalBody event)) $
			setconnected True
	match statusmatcher $ \event -> handleevent (signalBody event)
  where
	connmatcher = matchAny
		{ matchInterface = Just "org.wicd.daemon"
		, matchMember = Just "ConnectResultsSent"
		}
	statusmatcher = matchAny
		{ matchInterface = Just "org.wicd.daemon"
		, matchMember = Just "StatusChanged"
		}
	wicd_success = toVariant ("success" :: String)
	wicd_disconnected = toVariant [toVariant ("" :: String)]
	handleevent status
		| any (== wicd_disconnected) status = setconnected False
		| otherwise = noop
	match matcher a = 
		void $ addMatch client matcher a
#endif

handleConnection :: Assistant ()
handleConnection = do
	liftIO . sendNotification . networkConnectedNotifier =<< getDaemonStatus
	reconnectRemotes =<< networkRemotes

{- Network remotes to sync with. -}
networkRemotes :: Assistant [Remote]
networkRemotes = filter (isNothing . Remote.localpath) . syncRemotes
	<$> getDaemonStatus
