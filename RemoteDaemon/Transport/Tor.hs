{- git-remote-daemon, tor hidden service server and transport
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport.Tor (server, transport) where

import Common
import qualified Annex
import Annex.Concurrent
import Annex.ChangedRefs
import RemoteDaemon.Types
import RemoteDaemon.Common
import Utility.Tor
import Utility.AuthToken
import P2P.Protocol as P2P
import P2P.IO
import P2P.Annex
import P2P.Auth
import P2P.Address
import Annex.UUID
import Types.UUID
import Messages
import Git
import Git.Command

import System.PosixCompat.User
import Control.Concurrent
import System.Log.Logger (debugM)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.Async

-- Run tor hidden service.
server :: TransportHandle -> IO ()
server th@(TransportHandle (LocalRepo r) _) = do
	u <- liftAnnex th getUUID
	uid <- getRealUserID
	let ident = fromUUID u
	go u =<< getHiddenServiceSocketFile torAppName uid ident
  where
	go u (Just sock) = do
		q <- newTBMQueueIO maxConnections
		replicateM_ maxConnections $
			forkIO $ forever $ serveClient th u r q

		debugM "remotedaemon" "Tor hidden service running"
		serveUnixSocket sock $ \conn -> do
			ok <- atomically $ ifM (isFullTBMQueue q)
				( return False
				, do
					writeTBMQueue q conn
					return True
				)
			unless ok $ do
				hClose conn
				warningIO "dropped Tor connection, too busy"
	go _ Nothing = debugM "remotedaemon" "Tor hidden service not enabled"

-- How many clients to serve at a time, maximum. This is to avoid DOS attacks.
maxConnections :: Int
maxConnections = 100

serveClient :: TransportHandle -> UUID -> Repo -> TBMQueue Handle -> IO ()
serveClient th u r q = bracket setup cleanup start
  where
	setup = do
		h <- atomically $ readTBMQueue q
		debugM "remotedaemon" "serving a Tor connection"
		return h
	
	cleanup Nothing = return ()
	cleanup (Just h) = do
		debugM "remotedaemon" "done with Tor connection"
		hClose h

	start Nothing = return ()
	start (Just h) = do
		-- Avoid doing any work in the liftAnnex, since only one
		-- can run at a time.
		st <- liftAnnex th dupState
		((), st') <- Annex.run st $ do
			-- Load auth tokens for every connection, to notice
			-- when the allowed set is changed.
			allowed <- loadP2PAuthTokens
			let conn = P2PConnection
				{ connRepo = r
				, connCheckAuth = (`isAllowedAuthToken` allowed)
				, connIhdl = h
				, connOhdl = h
				}
			v <- liftIO $ runNetProto conn $ P2P.serveAuth u
			case v of
				Right (Just theiruuid) -> authed conn theiruuid
				Right Nothing -> liftIO $
					debugM "remotedaemon" "Tor connection failed to authenticate"
				Left e -> liftIO $
					debugM "remotedaemon" ("Tor connection error before authentication: " ++ e)
		-- Merge the duplicated state back in.
		liftAnnex th $ mergeState st'
	
	authed conn theiruuid = 
		bracket watchChangedRefs (liftIO . maybe noop stopWatchingChangedRefs) $ \crh -> do
			v' <- runFullProto (Serving theiruuid crh) conn $
				P2P.serveAuthed u
			case v' of
				Right () -> return ()
				Left e -> liftIO $ debugM "remotedaemon" ("Tor connection error: " ++ e)

-- Connect to peer's tor hidden service.
transport :: Transport
transport (RemoteRepo r _) url@(RemoteURI uri) th ichan ochan =
	case unformatP2PAddress (show uri) of
		Nothing -> return ()
		Just addr -> robustConnection 1 $ do
			g <- liftAnnex th Annex.gitRepo
			bracket (connectPeer g addr) closeConnection (go addr)
  where
	go addr conn = do
		myuuid <- liftAnnex th getUUID
		authtoken <- fromMaybe nullAuthToken
			<$> liftAnnex th (loadP2PRemoteAuthToken addr)
		res <- runNetProto conn $
			P2P.auth myuuid authtoken
		case res of
			Right (Just theiruuid) -> do
				expecteduuid <- liftAnnex th $ getRepoUUID r
				if expecteduuid == theiruuid
					then do
						send (CONNECTED url)
						status <- handlecontrol
							`race` handlepeer conn
						send (DISCONNECTED url)
						return $ either id id status
					else return ConnectionStopping
			_ -> return ConnectionClosed
	
	send msg = atomically $ writeTChan ochan msg
	
	handlecontrol = do
		msg <- atomically $ readTChan ichan
		case msg of
			STOP -> return ConnectionStopping
			LOSTNET -> return ConnectionStopping
			_ -> handlecontrol

	handlepeer conn = do
		v <- runNetProto conn P2P.notifyChange
		case v of
			Right (Just (ChangedRefs shas)) -> do
				whenM (checkNewShas th shas) $
					fetch
				handlepeer conn
			_ -> return ConnectionClosed
	
	fetch = do
		send (SYNCING url)
		ok <- inLocalRepo th $
			runBool [Param "fetch", Param $ Git.repoDescribe r]
		send (DONESYNCING url ok)
