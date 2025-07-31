{- P2P protocol, IO implementation
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings, CPP #-}

module P2P.IO
	( RunProto
	, RunState(..)
	, mkRunState
	, P2PHandle(..)
	, P2PConnection(..)
	, ConnIdent(..)
	, ClosableConnection(..)
	, stdioP2PConnection
	, stdioP2PConnectionDupped
	, connectPeer
	, closeConnection
	, serveUnixSocket
	, serveUnixSocket'
	, listenUnixSocket
	, ProtoFailure(..)
	, describeProtoFailure
	, runNetProto
	, runNet
	, signalFullyConsumedByteString
	) where

import Common
import P2P.Protocol
import P2P.Address
import P2P.Generic
import Git
import Git.Command
import Utility.AuthToken
import Utility.SimpleProtocol
import Utility.Metered
import Utility.Tor
import Utility.FileMode
import Utility.Debug
import Utility.MonotonicClock
import Types.UUID
import Annex.ChangedRefs

import Control.Monad.Free
import Control.Monad.IO.Class
import System.IO.Error
import Network.Socket
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as S
import System.PosixCompat.Files (groupReadMode, groupWriteMode, otherReadMode, otherWriteMode)

-- Type of interpreters of the Proto free monad.
type RunProto m = forall a. Proto a -> m (Either ProtoFailure a)

data ProtoFailure
	= ProtoFailureMessage String
	| ProtoFailureException SomeException
	| ProtoFailureIOError IOError
	deriving (Show)

describeProtoFailure :: ProtoFailure -> String
describeProtoFailure (ProtoFailureMessage s) = s
describeProtoFailure (ProtoFailureException e) = show e
describeProtoFailure (ProtoFailureIOError e) = show e

data RunState
	= Serving UUID (Maybe ChangedRefsHandle) (TVar ProtocolVersion)
	| Client (TVar ProtocolVersion)

mkRunState :: (TVar ProtocolVersion -> RunState) -> IO RunState
mkRunState mk = do
	tvar <- newTVarIO defaultProtocolVersion
	return (mk tvar)

data P2PHandle
	= P2PHandle Handle
	| P2PHandleTMVar
		(TMVar (Either L.ByteString Message))
		(Maybe (TMVar ()))
		(TMVar ())

signalFullyConsumedByteString :: P2PHandle -> IO ()
signalFullyConsumedByteString (P2PHandle _) = return ()
signalFullyConsumedByteString (P2PHandleTMVar _ Nothing _) = return () 
signalFullyConsumedByteString (P2PHandleTMVar _ (Just waitv) closedv) = 
	atomically $ putTMVar waitv ()
		`orElse` readTMVar closedv

data P2PConnection = P2PConnection
	{ connRepo :: Maybe Repo
	, connCheckAuth :: (AuthToken -> Bool)
	, connIhdl :: P2PHandle
	, connOhdl :: P2PHandle
	, connProcess :: Maybe ProcessHandle
	, connIdent :: ConnIdent
	}

-- Identifier for a connection, only used for debugging.
newtype ConnIdent = ConnIdent (Maybe String)
	deriving (Show)

data ClosableConnection conn
	= OpenConnection conn
	| ClosedConnection

-- P2PConnection using stdio.
stdioP2PConnection :: Maybe Git.Repo -> P2PConnection
stdioP2PConnection g = P2PConnection
	{ connRepo = g
	, connCheckAuth = const False
	, connIhdl = P2PHandle stdin
	, connOhdl = P2PHandle stdout
	, connProcess = Nothing
	, connIdent = ConnIdent Nothing
	}

-- P2PConnection using stdio, but with the handles first duplicated,
-- to avoid anything that might output to stdio (eg a program run by a
-- special remote) from interfering with the connection.
stdioP2PConnectionDupped :: Maybe Git.Repo -> IO P2PConnection
stdioP2PConnectionDupped g = do
	(readh, writeh) <- dupIoHandles
	return $ P2PConnection
		{ connRepo = g
		, connCheckAuth = const False
		, connIhdl = P2PHandle readh
		, connOhdl = P2PHandle writeh
		, connProcess = Nothing
		, connIdent = ConnIdent Nothing
		}

-- Opens a connection to a peer. Does not authenticate with it.
connectPeer :: Maybe Git.Repo -> P2PAddress -> IO P2PConnection
connectPeer g (TorAnnex onionaddress onionport) = do
	h <- setupHandleFromSocket =<< connectHiddenService onionaddress onionport
	return $ P2PConnection
		{ connRepo = g
		, connCheckAuth = const False
		, connIhdl = P2PHandle h
		, connOhdl = P2PHandle h
		, connProcess = Nothing
		, connIdent = ConnIdent Nothing
		}
connectPeer g (P2PAnnex netname address) = do
	(hin, hout, pid) <- connectGenericP2P netname address
	return $ P2PConnection
		{ connRepo = g
		, connCheckAuth = const False
		, connIhdl = P2PHandle hout
		, connOhdl = P2PHandle hin
		, connProcess = Just pid
		, connIdent = ConnIdent Nothing
		}

closeConnection :: P2PConnection -> IO ()
closeConnection conn = do
	closehandle (connIhdl conn)
	closehandle (connOhdl conn)
	case connProcess conn of
		Nothing -> noop
		Just ph -> void $ waitForProcess ph
  where
	closehandle (P2PHandle h) = hClose h
	closehandle (P2PHandleTMVar _ _ closedv) = 
		atomically $ void $ tryPutTMVar closedv ()

-- Serves the protocol on a unix socket.
--
-- The callback is run to serve a connection, and is responsible for
-- closing the Handle when done.
--
-- Note that while the callback is running, other connections won't be
-- processed, so longterm work should be run in a separate thread by
-- the callback.
serveUnixSocket :: OsPath -> (Handle -> IO ()) -> IO ()
serveUnixSocket unixsocket serveconn = do
	sock <- listenUnixSocket unixsocket
	serveUnixSocket' sock serveconn

serveUnixSocket' :: S.Socket -> (Handle -> IO ()) -> IO ()
serveUnixSocket' soc serveconn =
	forever $ do
		(conn, _) <- S.accept soc
		setupHandleFromSocket conn >>= serveconn

listenUnixSocket :: OsPath -> IO S.Socket
listenUnixSocket unixsocket = do
	removeWhenExistsWith removeFile unixsocket
	soc <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
	S.bind soc (S.SockAddrUnix (fromOsPath unixsocket))
	-- Allow everyone to read and write to the socket,
	-- so a daemon like tor, that is probably running as a different
	-- user, can access it.
	-- 
        -- Connections have to authenticate to do anything,
        -- so it's fine that other local users can connect to the
        -- socket.
	modifyFileMode unixsocket $ addModes
		[groupReadMode, groupWriteMode, otherReadMode, otherWriteMode]
	S.listen soc 2
	return soc

setupHandleFromSocket :: Socket -> IO Handle
setupHandleFromSocket s = do
	h <- socketToHandle s ReadWriteMode
	hSetBuffering h LineBuffering
	hSetBinaryMode h False
	return h

-- Purposefully incomplete interpreter of Proto.
--
-- This only runs Net actions. No Local actions will be run
-- (those need the Annex monad).
runNetProto :: RunState -> P2PConnection -> Proto a -> IO (Either ProtoFailure a)
runNetProto runst conn = go
  where
	go :: RunProto IO
	go (Pure v) = return (Right v)
	go (Free (Net n)) = runNet runst conn go n
	go (Free (Local _)) = return $ Left $
		ProtoFailureMessage "unexpected annex operation attempted"

-- Interpreter of the Net part of Proto.
--
-- An interpreter of Proto has to be provided, to handle the rest of Proto
-- actions.
runNet :: (MonadIO m, MonadMask m) => RunState -> P2PConnection -> RunProto m -> NetF (Proto a) -> m (Either ProtoFailure a)
runNet runst conn runner f = case f of
	SendMessage m next -> do
		v <- liftIO $ do
			debugMessage conn "P2P >" m
			case connOhdl conn of
				P2PHandle h -> tryNonAsync $ do
					hPutStrLn h $ unwords (formatMessage m)
					hFlush h
				P2PHandleTMVar mv _ closedv -> tryNonAsync $
					atomically $ putTMVar mv (Right m)
						`orElse` readTMVar closedv
		case v of
			Left e -> return $ Left $ ProtoFailureException e
			Right () -> runner next
	ReceiveMessage next ->
		let protoerr = return $ Left $
			ProtoFailureMessage "protocol error"
		    gotmessage m = do
			liftIO $ debugMessage conn "P2P <" m
			runner (next (Just m))
		in case connIhdl conn of
			P2PHandle h -> do
				v <- liftIO $ tryIOError $ getProtocolLine h
				case v of
					Left e -> return $ Left $ ProtoFailureIOError e
					Right Nothing -> protoerr
					Right (Just l) -> case parseMessage l of
						Just m -> gotmessage m
						Nothing -> runner (next Nothing)
			P2PHandleTMVar mv _ closedv -> do
				let recv = (Just <$> takeTMVar mv)
					`orElse` (readTMVar closedv >> return Nothing)
				liftIO (atomically recv) >>= \case
					Just (Right m) -> gotmessage m
					Just (Left _b) -> protoerr
					Nothing -> runner (next Nothing)
	SendBytes len b p next ->
		case connOhdl conn of
			P2PHandle h -> do
				v <- liftIO $ tryNonAsync $ do
					ok <- sendExactly len b h p
					hFlush h
					return ok
				case v of
					Right True -> runner next
					Right False -> return $ Left $
						ProtoFailureMessage "short data write"
					Left e -> return $ Left $ ProtoFailureException e
			P2PHandleTMVar mv waitv closedv -> do
				liftIO $ atomically $ putTMVar mv (Left b)
					`orElse` readTMVar closedv
				-- Wait for the whole bytestring to
				-- be processed.
				case waitv of
					Nothing -> noop
					Just v -> liftIO $ atomically $
						takeTMVar v
							`orElse` readTMVar closedv
				runner next
	ReceiveBytes len p next ->
		case connIhdl conn of
			P2PHandle h -> do
				v <- liftIO $ tryNonAsync $ receiveExactly len h p
				case v of
					Right b -> runner (next b)
					Left e -> return $ Left $
						ProtoFailureException e
			P2PHandleTMVar mv _ closedv -> do
				let recv = (Just <$> takeTMVar mv)
					`orElse` (readTMVar closedv >> return Nothing)
				liftIO (atomically recv) >>= \case
					Just (Left b) -> runner (next b)
					Just (Right _) -> return $ Left $
						ProtoFailureMessage "protocol error"
					Nothing -> return $ Left $
						ProtoFailureMessage "connection closed"
	CheckAuthToken _u t next -> do
		let authed = connCheckAuth conn t
		runner (next authed)
	Relay hin hout next -> do
		v <- liftIO $ runRelay runnerio hin hout
		case v of
			Left e -> return $ Left e
			Right exitcode -> runner (next exitcode)
	RelayService service next -> do
		v <- liftIO $ runRelayService conn runnerio service
		case v of
			Left e -> return $ Left e
			Right () -> runner next
	SetProtocolVersion v next -> do
		liftIO $ atomically $ writeTVar versiontvar v
		runner next
	GetProtocolVersion next ->
		liftIO (readTVarIO versiontvar) >>= runner . next
	GetMonotonicTimestamp next ->
		liftIO currentMonotonicTimestamp >>= runner . next
  where
	-- This is only used for running Net actions when relaying,
	-- so it's ok to use runNetProto, despite it not supporting
	-- all Proto actions.
	runnerio = runNetProto runst conn
	versiontvar = case runst of
		Serving _ _ tv -> tv
		Client tv -> tv

debugMessage :: P2PConnection -> String -> Message -> IO ()
debugMessage conn prefix m = do
	tid <- myThreadId	
	debug "P2P.IO" $ concat $ catMaybes $
		[ (\ident -> "[" ++ ident ++ "] ") <$> mident
		, Just $ "[" ++ show tid ++ "] "
		, Just $ prefix ++ " " ++ unwords (formatMessage safem)
		]
  where
	safem = case m of
		AUTH u _ -> AUTH u nullAuthToken
		_ -> m
	ConnIdent mident = connIdent conn

-- Send exactly the specified number of bytes or returns False.
--
-- The ByteString can be larger or smaller than the specified length.
-- For example, it can be lazily streaming from a file that gets
-- appended to, or truncated.
--
-- Must avoid sending too many bytes as it would confuse the other end.
-- This is easily dealt with by truncating it.
--
-- However, the whole ByteString will be evaluated here, even if
-- the end of it does not get sent.
--
-- If too few bytes are sent, the only option is to give up on this
-- connection. False is returned to indicate this problem.
sendExactly :: Len -> L.ByteString -> Handle -> MeterUpdate -> IO Bool
sendExactly (Len n) b h p = do
	let (x, y) = L.splitAt (fromIntegral n) b
	sent <- meteredWrite' p (B.hPut h) x
	L.length y `seq` return (fromBytesProcessed sent == n)

receiveExactly :: Len -> Handle -> MeterUpdate -> IO L.ByteString
receiveExactly (Len n) h p = hGetMetered h (Just n) p

runRelay :: RunProto IO -> RelayHandle -> RelayHandle -> IO (Either ProtoFailure ExitCode)
runRelay runner (RelayHandle hout) (RelayHandle hin) = 
	bracket setup cleanup go
		`catchNonAsync` (return . Left . ProtoFailureException)
  where
	setup = do
		v <- newEmptyMVar
		t1 <- async $ relayFeeder runner v hin
		t2 <- async $ relayReader v hout
		return (v, t1, t2)
	
	cleanup (_, t1, t2) = do
		hClose hin
		hClose hout
		cancel t1
		cancel t2
	
	go (v, _, _) = relayHelper runner v

runRelayService :: P2PConnection -> RunProto IO -> Service -> IO (Either ProtoFailure ())
runRelayService conn runner service = case connRepo conn of
	Just repo -> withCreateProcess (serviceproc' repo) go
		`catchNonAsync` (return . Left . ProtoFailureException)
	Nothing -> return $ Left $ ProtoFailureMessage
		"relaying to git not supported on this connection"
  where
	cmd = case service of
		UploadPack -> "upload-pack"
		ReceivePack -> "receive-pack"
	
	serviceproc repo = gitCreateProcess
		[ Param cmd
		, File (fromOsPath (repoPath repo))
		] repo
	serviceproc' repo = (serviceproc repo)
		{ std_out = CreatePipe
		, std_in = CreatePipe
		}

	go (Just hin) (Just hout) _ pid = do
		v <- newEmptyMVar
		r <- withAsync (relayFeeder runner v hin) $ \_ ->
			withAsync (relayReader v hout) $ \_ ->
				withAsync (waitexit v pid) $ \_ -> do
					r <- runrelay v
					hClose hin
					hClose hout
					return r
		void $ waitForProcess pid
		return r
	go _ _ _ _ = error "internal"
	
	runrelay v = relayHelper runner v >>= \case
		Left e -> return $ Left e
		Right exitcode -> runner $
			net $ relayToPeer (RelayDone exitcode)

	waitexit v pid = putMVar v . RelayDone =<< waitForProcess pid

-- Processes RelayData as it is put into the MVar.
relayHelper :: RunProto IO -> MVar RelayData -> IO (Either ProtoFailure ExitCode)
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
	
	-- Wait for the first available chunk. Then, without blocking,
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
