{- git-remote-daemon, generic P2P protocol transports
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module RemoteDaemon.Transport.P2PGeneric (
	server,
	transport,
	serveConnections
) where

import qualified Annex
import Annex.Common
import Annex.Concurrent
import Annex.ChangedRefs
import Annex.Perms
import RemoteDaemon.Types
import RemoteDaemon.Common
import Utility.AuthToken
import Utility.Hash
import P2P.Protocol as P2P
import P2P.IO
import P2P.Annex
import P2P.Auth
import P2P.Address
import P2P.Generic
import Annex.UUID
import Git
import Git.Command
import qualified Utility.OsString as OS

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.Async
import Network.Socket (Socket)
import qualified Data.Set as S

server :: Server
server ichan th@(TransportHandle (LocalRepo r) _ _) = go S.empty
  where
	go alreadystarted = do
		u <- liftAnnex th getUUID
		newaddrs <- filter (`S.notMember` alreadystarted) 
			<$> liftAnnex th loadP2PAddresses
		started <- filterM (start u) newaddrs
		handlecontrol (S.fromList started <> alreadystarted)

	start _ (TorAnnex _ _) = pure False
	start u addr@(P2PAnnex netname@(P2PNetName netname') address) = do
		socketfile <- liftAnnex th $ getSocketFile netname address
		sock <- listenUnixSocket socketfile
		tryNonAsync (socketGenericP2P netname address socketfile) >>= \case
			Right _pid -> do
				debug' $ "listener started for P2P network " ++ netname'
				void $ async $ serveConnections
					(loadP2PAuthTokens addr)
					netname th u r sock
				return True
			Left err -> do
				liftAnnex th $ warning $
					"unable to start listener for P2P network "
						<> UnquotedString netname' 
						<> ": " <> UnquotedString (show err)
				return False
	
	handlecontrol started = do
		msg <- atomically $ readTChan ichan
		case msg of
			-- On reload, the configuration may have changed to
			-- enable a P2P network. Start any new ones.
			RELOAD -> go started
			_ -> handlecontrol started

getSocketFile :: P2PNetName -> UnderlyingP2PAddress -> Annex OsPath
getSocketFile (P2PNetName netname) (UnderlyingP2PAddress address) = do
	d <- fromRepo gitAnnexP2PDir
	createAnnexDirectory d
	-- Since unix socket path length is limited, use a md5sum of
	-- the netname and address.
	let f = d </> toOsPath (show (md5 (encodeBL (netname ++ ":" ++ address))))
	-- Use whichever is shorter of the absolute or relative path.
	relf <- liftIO $ relPathCwdToFile f
	absf <- liftIO $ absPath f
	if OS.length absf > OS.length relf
		then return relf
		else return absf

serveConnections
	:: Annex AllowedAuthTokens
	-> P2PNetName
	-> TransportHandle
	-> UUID
	-> Repo
	-> Socket
	-> IO ()
serveConnections loadauthtokens (P2PNetName netname) th u r sock = do
	q <- newTBMQueueIO maxConnections
	replicateM_ maxConnections $
		forkIO $ forever $ 
			serveClient loadauthtokens (P2PNetName netname) th u r q
	serveUnixSocket' sock $ \conn -> do
		ok <- atomically $ ifM (isFullTBMQueue q)
			( return False
			, do
				writeTBMQueue q conn
				return True
			)
		unless ok $ do
			hClose conn
			liftAnnex th $ warning $ 
				"dropped P2P network " 
					<> UnquotedString netname 
					<> " connection, too busy"

-- How many clients to serve at a time, maximum per P2P network.
-- This is to avoid DOS attacks.
maxConnections :: Int
maxConnections = 100

serveClient
	:: Annex AllowedAuthTokens
	-> P2PNetName
	-> TransportHandle
	-> UUID
	-> Repo
	-> TBMQueue Handle
	-> IO ()
serveClient loadauthtokens (P2PNetName netname) th@(TransportHandle _ _ rd) u r q = bracket setup cleanup start
  where
	setup = do
		h <- atomically $ readTBMQueue q
		debug' $ "serving a " ++ netname ++ " connection"
		return h
	
	cleanup Nothing = return ()
	cleanup (Just h) = do
		debug' $ "done with " ++ netname ++ " connection"
		hClose h

	start Nothing = return ()
	start (Just h) = do
		-- Avoid doing any work in the liftAnnex, since only one
		-- can run at a time.
		st <- liftAnnex th dupState
		((), (st', _rd)) <- Annex.run (st, rd) $ do
			-- Load auth tokens for every connection, to notice
			-- when the allowed set is changed.
			allowed <- loadauthtokens
			let conn = P2PConnection
				{ connRepo = Just r
				, connCheckAuth = (`isAllowedAuthToken` allowed)
				, connIhdl = P2PHandle h
				, connOhdl = P2PHandle h
				, connProcess = Nothing
				, connIdent = ConnIdent $ Just $
					netname ++ " remotedaemon"
				}
			-- not really Client, but we don't know their uuid yet
			runstauth <- liftIO $ mkRunState Client
			v <- liftIO $ runNetProto runstauth conn $ P2P.serveAuth u
			case v of
				Right (Just theiruuid) -> authed conn theiruuid
				Right Nothing -> liftIO $ debug' $
					netname ++ " connection failed to authenticate"
				Left e -> liftIO $ debug' $
					netname ++ " connection error before authentication: " ++ describeProtoFailure e
		-- Merge the duplicated state back in.
		liftAnnex th $ mergeState st'
	
	authed conn theiruuid = 
		bracket watchChangedRefs (liftIO . maybe noop stopWatchingChangedRefs) $ \crh -> do
			runst <- liftIO $ mkRunState (Serving theiruuid crh)
			v' <- runFullProto runst conn $
				P2P.serveAuthed P2P.ServeReadWrite u
			case v' of
				Right () -> return ()
				Left e -> liftIO $ debug' $ 
					netname ++ " connection error: " ++ describeProtoFailure e

transport :: Transport
transport (RemoteRepo r gc) url@(RemoteURI uri) th ichan ochan =
	case unformatP2PAddress (show uri) of
		Nothing -> return ()
		Just addr -> robustConnection 1 $ do
			g <- liftAnnex th Annex.gitRepo
			bracket (connectPeer (Just g) addr) closeConnection (go addr)
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

debug' :: String -> IO ()
debug' = debug "RemoteDaemon.Transport.P2PGeneric"
