{- P2P protocol, IO implementation
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts, BangPatterns, CPP #-}

module P2P.IO
	( RunProto
	, P2PConnection(..)
	, connectPeer
	, setupHandle
	, runNetProto
	, runNet
	) where

import P2P.Protocol
import P2P.Address
import Utility.Process
import Git
import Git.Command
import Utility.AuthToken
import Utility.SafeCommand
import Utility.SimpleProtocol
import Utility.Exception
import Utility.Tor
import Utility.FileSystemEncoding

import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import System.Exit (ExitCode(..))
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- Type of interpreters of the Proto free monad.
type RunProto m = forall a. (MonadIO m, MonadMask m) => Proto a -> m (Maybe a)

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
runNetProto :: P2PConnection -> Proto a -> IO (Maybe a)
runNetProto conn = go
  where
	go :: RunProto IO
	go (Pure v) = pure (Just v)
	go (Free (Net n)) = runNet conn go n
	go (Free (Local _)) = return Nothing

-- Interpreter of the Net part of Proto.
--
-- An interpreter of Proto has to be provided, to handle the rest of Proto
-- actions.
runNet :: (MonadIO m, MonadMask m) => P2PConnection -> RunProto m -> NetF (Proto a) -> m (Maybe a)
runNet conn runner f = case f of
	SendMessage m next -> do
		v <- liftIO $ tryNonAsync $ do
			hPutStrLn (connOhdl conn) (unwords (formatMessage m))
			hFlush (connOhdl conn)
		case v of
			Left _e -> return Nothing
			Right () -> runner next
	ReceiveMessage next -> do
		v <- liftIO $ tryNonAsync $ hGetLine (connIhdl conn)
		case v of
			Left _e -> return Nothing
			Right l -> case parseMessage l of
				Just m -> runner (next m)
				Nothing -> runner $ do
					let e = ERROR $ "protocol parse error: " ++ show l
					net $ sendMessage e
					next e
	SendBytes len b next -> do
		v <- liftIO $ tryNonAsync $ do
			ok <- sendExactly len b (connOhdl conn)
			hFlush (connOhdl conn)
			return ok
		case v of
			Right True -> runner next
			_ -> return Nothing
	ReceiveBytes (Len n) next -> do
		v <- liftIO $ tryNonAsync $ L.hGet (connIhdl conn) (fromIntegral n)
		case v of
			Left _e -> return Nothing
			Right b -> runner (next b)
	CheckAuthToken _u t next -> do
		let authed = connCheckAuth conn t
		runner (next authed)
	Relay hin hout next -> do
		v <- liftIO $ runRelay runnerio hin hout
		case v of
			Nothing -> return Nothing
			Just exitcode -> runner (next exitcode)
	RelayService service next -> do
		v <- liftIO $ runRelayService conn runnerio service
		case v of
			Nothing -> return Nothing
			Just () -> runner next
  where
	-- This is only used for running Net actions when relaying,
	-- so it's ok to use runNetProto, despite it not supporting
	-- all Proto actions.
	runnerio = runNetProto conn

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
--
-- We can't check the length of the whole lazy bytestring without buffering
-- it in memory. Instead, process it one chunk at a time, and sum the length
-- of the chunks.
sendExactly :: Len -> L.ByteString -> Handle -> IO Bool
sendExactly (Len l) lb h = go 0 $ L.toChunks $ L.take (fromIntegral l) lb
  where
	go n [] = return (toInteger n == l)
	go n (b:bs) = do
		B.hPut h b
		let !n' = n + B.length b
		go n' bs

runRelay :: RunProto IO -> RelayHandle -> RelayHandle -> IO (Maybe ExitCode)
runRelay runner (RelayHandle hout) (RelayHandle hin) = bracket setup cleanup go
  where
	setup = do
		v <- newEmptyMVar
		void $ async $ relayFeeder runner v
		void $ async $ relayReader v hout
		return v
	
	cleanup _ = do
		hClose hin
		hClose hout
	
	go v = relayHelper runner v hin

runRelayService :: P2PConnection -> RunProto IO -> Service -> IO (Maybe ())
runRelayService conn runner service = bracket setup cleanup go
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
		void $ async $ relayFeeder runner v
		void $ async $ relayReader v hout
		waiter <- async $ waitexit v pid
		return (v, waiter, hin, hout, pid)

	cleanup (_, waiter, hin, hout, pid) = do
		hClose hin
		hClose hout
		cancel waiter
		void $ waitForProcess pid

	go (v, _, hin, _, _) = do
		r <- relayHelper runner v hin
		case r of
			Nothing -> return Nothing
			Just exitcode -> runner $ net $ relayToPeer (RelayDone exitcode)
	
	waitexit v pid = putMVar v . RelayDone =<< waitForProcess pid

-- Processes RelayData as it is put into the MVar.
relayHelper :: RunProto IO -> MVar RelayData -> Handle -> IO (Maybe ExitCode)
relayHelper runner v hin = loop
  where
	loop = do
		d <- takeMVar v
		case d of
			RelayFromPeer b -> do
				L.hPut hin b
				hFlush hin
				loop
			RelayToPeer b -> do
				r <- runner $ net $ relayToPeer (RelayToPeer b)
				case r of
					Nothing -> return Nothing
					Just () -> loop
			RelayDone exitcode -> do
				_ <- runner $ net $ relayToPeer (RelayDone exitcode)
				return (Just exitcode)

-- Takes input from the peer, and puts it into the MVar for processing.
-- Repeats until the peer tells it it's done or hangs up.
relayFeeder :: RunProto IO -> MVar RelayData -> IO ()
relayFeeder runner v = loop
  where
	loop = do
		mrd <- runner $ net relayFromPeer
		case mrd of
			Nothing -> putMVar v (RelayDone (ExitFailure 1))
			Just rd -> do
				putMVar v rd
				case rd of
					RelayDone _ -> return ()
					_ -> loop

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
