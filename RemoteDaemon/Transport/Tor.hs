{- git-remote-daemon, tor hidden service transport
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport.Tor (server) where

import Common
import RemoteDaemon.Types
import RemoteDaemon.Common
import Utility.Tor
import Utility.FileMode
import Remote.Helper.Tor
import Remote.Helper.P2P
import Remote.Helper.P2P.IO
import Annex.UUID
import Types.UUID

import System.PosixCompat.User
import Network.Socket
import Control.Concurrent
import System.Log.Logger (debugM)

-- Run tor hidden service.
server :: TransportHandle -> IO ()
server th@(TransportHandle (LocalRepo r) _) = do
	u <- liftAnnex th getUUID
	uid <- getRealUserID
	let ident = fromUUID u
	let sock = socketFile uid ident
	nukeFile sock
	soc <- socket AF_UNIX Stream defaultProtocol
	bind soc (SockAddrUnix sock)
	-- Allow everyone to read and write to the socket; tor is probably
	-- running as a different user. Connections have to authenticate
	-- to do anything, so it's fine that other local users can connect.
	modifyFileMode sock $ addModes
		[groupReadMode, groupWriteMode, otherReadMode, otherWriteMode]
	listen soc 2
	debugM "remotedaemon" "tor hidden service running"
	forever $ do
		(conn, _) <- accept soc
		forkIO $ do
			debugM "remotedaemon" "handling a connection"
			h <- torHandle conn
			_ <- runNetProtoHandle h h r (serve u)
			hClose h
