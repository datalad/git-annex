{- P2P protocol, IO implementation
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts, CPP #-}

module P2P.IO
	( RunProto
	, P2PConnection(..)
	, connectPeer
	, closeConnection
	, serveUnixSocket
	, setupHandle
	, runNetProto
	, runNet
	) where

import Common
import P2P.Protocol
import P2P.Address
import Git
import Git.Command
import Utility.AuthToken
import Utility.SimpleProtocol
import Utility.Metered
import Utility.Tor
import Utility.FileMode

import Control.Monad.Free
import Control.Monad.IO.Class
import System.Exit (ExitCode(..))
import Network.Socket
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.Log.Logger (debugM)
import qualified Network.Socket as S

-- Type of interpreters of the Proto free monad.
type RunProto m = forall a. (MonadIO m, MonadMask m) => Proto a -> m (Either String a)

data P2PConnection = P2PConnection
	{ connRepo :: Repo
	, connCheckAuth :: (AuthToken -> Bool)
	, connIhdl :: Handle
	, connOhdl :: Handle
	}

-- Opens a connection to a peer. Does not authenticate with it.
connectPeer :: Git.Repo -> P2PAddress -> IO P2PConnection
connectPeer g (TorAnnex onionaddress onionport) = do
	h <- setupHandle =<< connectHiddenService onionaddress onionport
	return $ P2PConnection
		{ connRepo = g
		, connCheckAuth = const False
		, connIhdl = h
		, connOhdl = h
		}

closeConnection :: P2PConnection -> IO ()
closeConnection conn = do
	hClose (connIhdl conn)
	hClose (connOhdl conn)

-- Serves the protocol on a unix socket.
--
-- The callback is run to serve a connection, and is responsible for
-- closing the Handle when done.
--
-- Note that while the callback is running, other connections won't be
-- processed, so longterm work should be run in a separate thread by
-- the callback.
serveUnixSocket :: FilePath -> (Handle -> IO ()) -> IO ()
serveUnixSocket unixsocket serveconn = do
	nukeFile unixsocket
	soc <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
	S.bind soc (S.SockAddrUnix unixsocket)
	-- Allow everyone to read and write to the socket,
	-- so a daemon like tor, that is probably running as a different
	-- de sock $ addModes
	-- user, can access it.
	-- 
        -- Connections have to authenticate to do anything,
        -- so it's fine that other local users can connect to the
        -- socket.
	modifyFileMode unixsocket $ addModes
		[groupReadMode, groupWriteMode, otherReadMode, otherWriteMode]
	S.listen soc 2
	forever $ do
		(conn, _) <- S.accept soc
		setupHandle conn >>= serveconn

setupHandle :: Socket -> IO Handle
setupHandle s = do
	h <- socketToHandle s ReadWriteMode
	hSetBuffering h LineBuffering
	hSetBinaryMode h False
	fileEncoding h
	return h

-- Purposefully incomplete interpreter of Proto.
--
-- This only runs Net actions. No Local actions will be run
-- (those need the Annex monad) -- if the interpreter reaches any,
-- it returns Nothing.
runNetProto :: P2PConnection -> Proto a -> IO (Either String a)
runNetProto conn = go
  where
	go :: RunProto IO
	go (Pure v) = return (Right v)
	go (Free (Net n)) = runNet conn go n
	go (Free (Local _)) = return (Left "unexpected annex operation attempted")

-- Interpreter of the Net part of Proto.
--
-- An interpreter of Proto has to be provided, to handle the rest of Proto
-- actions.
runNet :: (MonadIO m, MonadMask m) => P2PConnection -> RunProto m -> NetF (Proto a) -> m (Either String a)
runNet conn runner f = case f of
	SendMessage m next -> do
		v <- liftIO $ tryNonAsync $ do
			let l = unwords (formatMessage m)
			debugMessage "P2P >" m
			hPutStrLn (connOhdl conn) l
			hFlush (connOhdl conn)
		case v of
			Left e -> return (Left (show e))
			Right () -> runner next
	ReceiveMessage next -> do
		v <- liftIO $ tryNonAsync $ getProtocolLine (connIhdl conn)
		case v of
			Left e -> return (Left (show e))
			Right Nothing -> return (Left "protocol error")
			Right (Just l) -> case parseMessage l of
					Just m -> do
						liftIO $ debugMessage "P2P <" m
						runner (next m)
					Nothing -> runner $ do
						let e = ERROR $ "protocol parse error: " ++ show l
						net $ sendMessage e
						next e
	SendBytes len b p next -> do
		v <- liftIO $ tryNonAsync $ do
			ok <- sendExactly len b (connOhdl conn) p
			hFlush (connOhdl conn)
			return ok
		case v of
			Right True -> runner next
			Right False -> return (Left "short data write")
			Left e -> return (Left (show e))
	ReceiveBytes len p next -> do
		v <- liftIO $ tryNonAsync $ receiveExactly len (connIhdl conn) p
		case v of
			Left e -> return (Left (show e))
			Right b -> runner (next b)
	CheckAuthToken _u t next -> do
		let authed = connCheckAuth conn t
		runner (next authed)
	Relay hin hout next -> do
		v <- liftIO $ runRelay runnerio hin hout
		case v of
			Left e -> return (Left e)
			Right exitcode -> runner (next exitcode)
	RelayService service next -> do
		v <- liftIO $ runRelayService conn runnerio service
		case v of
			Left e -> return (Left e)
			Right () -> runner next
  where
	-- This is only used for running Net actions when relaying,
	-- so it's ok to use runNetProto, despite it not supporting
	-- all Proto actions.
	runnerio = runNetProto conn

debugMessage :: String -> Message -> IO ()
debugMessage prefix m = debugM "p2p" $
	prefix ++ " " ++ unwords (formatMessage safem)
  where
	safem = case m of
		AUTH u _ -> AUTH u nullAuthToken
		_ -> m

-- Send exactly the specified number of bytes or returns False.
--
-- The ByteString can be larger or smaller than the specified length.
-- For example, it can be lazily streaming from a file that gets
-- appended to, or truncated.
--
-- Must avoid sending too many bytes as it would confuse the other end.
-- This is easily dealt with by truncating it.
--
-- If too few bytes are sent, the only option is to give up on this
-- connection. False is returned to indicate this problem.
sendExactly :: Len -> L.ByteString -> Handle -> MeterUpdate -> IO Bool
sendExactly (Len n) b h p = do
	sent <- meteredWrite' p h (L.take (fromIntegral n) b)
	return (fromBytesProcessed sent == n)

receiveExactly :: Len -> Handle -> MeterUpdate -> IO L.ByteString
receiveExactly (Len n) h p = hGetMetered h (Just n) p

runRelay :: RunProto IO -> RelayHandle -> RelayHandle -> IO (Either String ExitCode)
runRelay runner (RelayHandle hout) (RelayHandle hin) = 
	bracket setup cleanup go
		`catchNonAsync` (return . Left . show)
  where
	setup = do
		v <- newEmptyMVar
		void $ async $ relayFeeder runner v hin
		void $ async $ relayReader v hout
		return v
	
	cleanup _ = do
		hClose hin
		hClose hout
	
	go v = relayHelper runner v

runRelayService :: P2PConnection -> RunProto IO -> Service -> IO (Either String ())
runRelayService conn runner service = 
	bracket setup cleanup go
		`catchNonAsync` (return . Left . show)
  where
	cmd = case service of
		UploadPack -> "upload-pack"
		ReceivePack -> "receive-pack"
	
	serviceproc = gitCreateProcess
		[ Param cmd
		, File (repoPath (connRepo conn))
		] (connRepo conn)

	setup = do
		(Just hin, Just hout, _, pid) <- createProcess serviceproc
			{ std_out = CreatePipe
			, std_in = CreatePipe
			}
		v <- newEmptyMVar
		void $ async $ relayFeeder runner v hin
		void $ async $ relayReader v hout
		waiter <- async $ waitexit v pid
		return (v, waiter, hin, hout, pid)

	cleanup (_, waiter, hin, hout, pid) = do
		hClose hin
		hClose hout
		cancel waiter
		void $ waitForProcess pid

	go (v, _, _, _, _) = do
		r <- relayHelper runner v
		case r of
			Left e -> return (Left (show e))
			Right exitcode -> runner $ net $ relayToPeer (RelayDone exitcode)
	
	waitexit v pid = putMVar v . RelayDone =<< waitForProcess pid

-- Processes RelayData as it is put into the MVar.
relayHelper :: RunProto IO -> MVar RelayData -> IO (Either String ExitCode)
relayHelper runner v = loop
  where
	loop = do
		d <- takeMVar v
		case d of
			RelayToPeer b -> do
				r <- runner $ net $ relayToPeer (RelayToPeer b)
				case r of
					Left e -> return (Left e)
					Right () -> loop
			RelayDone exitcode -> do
				_ <- runner $ net $ relayToPeer (RelayDone exitcode)
				return (Right exitcode)
			RelayFromPeer _ -> loop -- not handled here

-- Takes input from the peer, and sends it to the relay process's stdin.
-- Repeats until the peer tells it it's done or hangs up.
relayFeeder :: RunProto IO -> MVar RelayData -> Handle -> IO ()
relayFeeder runner v hin = loop
  where
	loop = do
		mrd <- runner $ net relayFromPeer
		case mrd of
			Left _e ->
				putMVar v (RelayDone (ExitFailure 1))
			Right (RelayDone exitcode) ->
				putMVar v (RelayDone exitcode)
			Right (RelayFromPeer b) -> do
				L.hPut hin b
				hFlush hin
				loop
			Right (RelayToPeer _) -> loop -- not handled here

-- Reads input from the Handle and puts it into the MVar for relaying to
-- the peer. Continues until EOF on the Handle.
relayReader :: MVar RelayData -> Handle -> IO ()
relayReader v hout = loop
  where
	loop = do
		bs <- getsome []
		case bs of
			[] -> return ()
			_ -> do
				putMVar v $ RelayToPeer (L.fromChunks bs)
				loop
	
	-- Waiit for the first available chunk. Then, without blocking,
	-- try to get more chunks, in case a stream of chunks is being
	-- written in close succession. 
	--
	-- On Windows, hGetNonBlocking is broken, so avoid using it there.
	getsome [] = do
		b <- B.hGetSome hout chunk
		if B.null b
			then return []
#ifndef mingw32_HOST_OS
			else getsome [b]
#else
			else return [b]
#endif
	getsome bs = do
		b <- B.hGetNonBlocking hout chunk
		if B.null b
			then return (reverse bs)
			else getsome (b:bs)
	
	chunk = 65536
