{- git-remote-daemon, tor hidden service transport
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport.Tor (server) where

import Common
import qualified Annex
import Annex.Concurrent
import RemoteDaemon.Types
import RemoteDaemon.Common
import Utility.Tor
import Utility.FileMode
import Utility.AuthToken
import P2P.Protocol
import P2P.IO
import P2P.Annex
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
		h <- setupHandle conn
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
			v <- liftIO $ runNetProto conn $ serveAuth u
			case v of
				Right (Just theiruuid) -> void $ 
					runFullProto (Serving theiruuid) conn $
						serveAuthed u
				Right Nothing -> do
					liftIO $ debugM "remotedaemon" "TOR connection failed to authenticate"
					return ()
				Left e -> do
					warning e
					return ()
		-- Merge the duplicated state back in.
		liftAnnex th $ mergeState st'
		debugM "remotedaemon" "done with TOR connection"
