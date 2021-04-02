{- git-remote-daemon, tor hidden service server and transport
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module RemoteDaemon.Transport.Tor (server, transport, torSocketFile) where

import Common
import qualified Annex
import Annex.Concurrent
import Annex.ChangedRefs
import RemoteDaemon.Types
import RemoteDaemon.Common
import Utility.AuthToken
import Utility.Tor
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

import Control.Concurrent
import System.Log.Logger (debugM)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.Async
#ifndef mingw32_HOST_OS
import System.Posix.User
#endif

-- Run tor hidden service.
server :: Server
server ichan th@(TransportHandle (LocalRepo r) _ _) = go
  where
	go = checkstartservice >>= handlecontrol

	checkstartservice = do
		u <- liftAnnex th getUUID
		msock <- liftAnnex th torSocketFile
		case msock of
			Nothing -> do
				debugM "remotedaemon" "Tor hidden service not enabled"
				return False
			Just sock -> do
				void $ async $ startservice sock u
				return True
	
	startservice sock u = do
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
				liftAnnex th $ warning "dropped Tor connection, too busy"
	
	handlecontrol servicerunning = do
		msg <- atomically $ readTChan ichan
		case msg of
			-- On reload, the configuration may have changed to
			-- enable the tor hidden service. If it was not
			-- enabled before, start it,
			RELOAD | not servicerunning -> go
			-- We can ignore all other messages; no need
			-- to restart the hidden service when the network
			-- changes as tor takes care of all that.
			_ -> handlecontrol servicerunning

-- How many clients to serve at a time, maximum. This is to avoid DOS attacks.
maxConnections :: Int
maxConnections = 100

serveClient :: TransportHandle -> UUID -> Repo -> TBMQueue Handle -> IO ()
serveClient th@(TransportHandle _ _ rd) u r q = bracket setup cleanup start
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
		((), (st', _rd)) <- Annex.run (st, rd) $ do
			-- Load auth tokens for every connection, to notice
			-- when the allowed set is changed.
			allowed <- loadP2PAuthTokens
			let conn = P2PConnection
				{ connRepo = r
				, connCheckAuth = (`isAllowedAuthToken` allowed)
				, connIhdl = h
				, connOhdl = h
				, connIdent = ConnIdent $ Just "tor remotedaemon"
				}
			-- not really Client, but we don't know their uuid yet
			runstauth <- liftIO $ mkRunState Client
			v <- liftIO $ runNetProto runstauth conn $ P2P.serveAuth u
			case v of
				Right (Just theiruuid) -> authed conn theiruuid
				Right Nothing -> liftIO $ debugM "remotedaemon"
					"Tor connection failed to authenticate"
				Left e -> liftIO $ debugM "remotedaemon" $
					"Tor connection error before authentication: " ++ describeProtoFailure e
		-- Merge the duplicated state back in.
		liftAnnex th $ mergeState st'
	
	authed conn theiruuid = 
		bracket watchChangedRefs (liftIO . maybe noop stopWatchingChangedRefs) $ \crh -> do
			runst <- liftIO $ mkRunState (Serving theiruuid crh)
			v' <- runFullProto runst conn $
				P2P.serveAuthed P2P.ServeReadWrite u
			case v' of
				Right () -> return ()
				Left e -> liftIO $ debugM "remotedaemon" $ 
					"Tor connection error: " ++ describeProtoFailure e

-- Connect to peer's tor hidden service.
transport :: Transport
transport (RemoteRepo r gc) url@(RemoteURI uri) th ichan ochan =
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
		runst <- mkRunState Client
		res <- runNetProto runst conn $ P2P.auth myuuid authtoken noop
		case res of
			Right (Just theiruuid) -> do
				expecteduuid <- liftAnnex th $ getRepoUUID r
				if expecteduuid == theiruuid
					then do
						send (CONNECTED url)
						status <- handlecontrol
							`race` handlepeer runst conn
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

	handlepeer runst conn = do
		v <- runNetProto runst conn P2P.notifyChange
		case v of
			Right (Just (ChangedRefs shas)) -> do
				whenM (checkShouldFetch gc th shas) $
					fetch
				handlepeer runst conn
			_ -> return ConnectionClosed
	
	fetch = do
		send (SYNCING url)
		ok <- inLocalRepo th $
			runBool [Param "fetch", Param $ Git.repoDescribe r]
		send (DONESYNCING url ok)

torSocketFile :: Annex.Annex (Maybe FilePath)
torSocketFile = do
	u <- getUUID
	let ident = fromUUID u
#ifndef mingw32_HOST_OS
	uid <- liftIO getRealUserID
#else
	let uid = 0
#endif
	liftIO $ getHiddenServiceSocketFile torAppName uid ident
