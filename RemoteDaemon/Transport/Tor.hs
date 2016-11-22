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
import Messages
import Git

import System.PosixCompat.User
import Network.Socket
import Control.Concurrent
import System.Log.Logger (debugM)
import Control.Concurrent.STM

-- Run tor hidden service.
server :: TransportHandle -> IO ()
server th@(TransportHandle (LocalRepo r) _) = do
	u <- liftAnnex th getUUID

	q <- newTBQueueIO maxConnections
	replicateM_ maxConnections $
		forkIO $ forever $ serveClient u r q

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
		h <- torHandle conn
		ok <- atomically $ ifM (isFullTBQueue q)
			( return False
			, do
				writeTBQueue q h
				return True
			)
		unless ok $ do
			hClose h
			warningIO "dropped TOR connection, too busy"

-- How many clients to serve at a time, maximum. This is to avoid DOS
-- attacks.
maxConnections :: Int
maxConnections = 10

serveClient :: UUID -> Repo -> TBQueue Handle -> IO ()
serveClient u r q = bracket setup cleanup go
  where
	setup = atomically $ readTBQueue q
	cleanup = hClose
	go h = do
		debugM "remotedaemon" "serving a TOR connection"
		void $ runNetProtoHandle h h r (serve u)
		debugM "remotedaemon" "done with TOR connection"
