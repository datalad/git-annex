{- git-remote-daemon, tor hidden service server and transport
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module RemoteDaemon.Transport.Tor (server, transport, torSocketFile) where

import Annex.Common
import RemoteDaemon.Types
import RemoteDaemon.Common
import Utility.Tor
import P2P.IO
import P2P.Auth
import P2P.Address
import Annex.UUID
import qualified RemoteDaemon.Transport.P2PGeneric as P2PGeneric

import Control.Concurrent.STM
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
			Nothing ->
				return False
			Just socketfile -> do
				void $ async $ startservice socketfile u
				return True
	
	startservice socketfile u = do
		sock <- listenUnixSocket socketfile
		P2PGeneric.serveConnections
			loadP2PAuthTokensTor
			(P2PNetName "tor")
			th u r sock

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

-- Connect to peer's tor hidden service. P2PGeneric can do this,
-- since it uses connectPeer which also supports tor.
transport :: Transport
transport = P2PGeneric.transport

torSocketFile :: Annex (Maybe OsPath)
torSocketFile = do
	u <- getUUID
	let ident = fromUUID u
#ifndef mingw32_HOST_OS
	uid <- liftIO getRealUserID
#else
	let uid = 0
#endif
	liftIO $ getHiddenServiceSocketFile torAppName uid ident
