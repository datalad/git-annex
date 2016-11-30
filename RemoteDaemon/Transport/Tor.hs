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
import Utility.AuthToken
import Remote.Helper.Tor
import P2P.Protocol
import P2P.IO
import P2P.Auth
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
		forkIO $ forever $ serveClient th u r q

	uid <- getRealUserID
	let ident = fromUUID u
	let sock = hiddenServiceSocketFile uid ident
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

serveClient :: TransportHandle -> UUID -> Repo -> TBQueue Handle -> IO ()
serveClient th u r q = bracket setup cleanup go
  where
	setup = atomically $ readTBQueue q
	cleanup = hClose
	go h = do
		debugM "remotedaemon" "serving a TOR connection"
		-- Load auth tokens for every connection, to notice
		-- when the allowed set is changed.
		allowed <- liftAnnex th loadP2PAuthTokens
		let runenv = RunEnv
			{ runRepo = r
			, runCheckAuth = (`isAllowedAuthToken` allowed)
			, runIhdl = h
			, runOhdl = h
			}
		void $ runNetProtoHandle runenv (serve u)
		debugM "remotedaemon" "done with TOR connection"
