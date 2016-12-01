{- P2P protocol, IO implementation
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts, CPP #-}

module P2P.IO
	( RunEnv(..)
	, runNetProto
	, runNet
	) where

import P2P.Protocol
import Utility.Process
import Git
import Git.Command
import Utility.AuthToken
import Utility.SafeCommand
import Utility.SimpleProtocol
import Utility.Exception

import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import System.Exit (ExitCode(..))
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- Type of interpreters of the Proto free monad.
type RunProto m = forall a. (MonadIO m, MonadMask m) => Proto a -> m (Maybe a)

data RunEnv = RunEnv
	{ runRepo :: Repo
	, runCheckAuth :: (AuthToken -> Bool)
	, runIhdl :: Handle
	, runOhdl :: Handle
	}

-- Purposefully incomplete interpreter of Proto.
--
-- This only runs Net actions. No Local actions will be run
-- (those need the Annex monad) -- if the interpreter reaches any,
-- it returns Nothing.
runNetProto :: RunEnv -> Proto a -> IO (Maybe a)
runNetProto runenv = go
  where
	go :: RunProto IO
	go (Pure v) = pure (Just v)
	go (Free (Net n)) = runNet runenv go n
	go (Free (Local _)) = return Nothing

-- Interprater of the Net part of Proto.
--
-- An interpreter of Proto has to be provided, to handle the rest of Proto
-- actions.
runNet :: (MonadIO m, MonadMask m) => RunEnv -> RunProto m -> NetF (Proto a) -> m (Maybe a)
runNet runenv runner f = case f of
	SendMessage m next -> do
		v <- liftIO $ tryIO $ do
			hPutStrLn (runOhdl runenv) (unwords (formatMessage m))
			hFlush (runOhdl runenv)
		case v of
			Left _e -> return Nothing
			Right () -> runner next
	ReceiveMessage next -> do
		v <- liftIO $ tryIO $ hGetLine (runIhdl runenv)
		case v of
			Left _e -> return Nothing
			Right l -> case parseMessage l of
				Just m -> runner (next m)
				Nothing -> runner $ do
					let e = ERROR $ "protocol parse error: " ++ show l
					net $ sendMessage e
					next e
	SendBytes _len b next -> do
		v <- liftIO $ tryIO $ do
			L.hPut (runOhdl runenv) b
			hFlush (runOhdl runenv)
		case v of
			Left _e -> return Nothing
			Right () -> runner next
	ReceiveBytes (Len n) next -> do
		v <- liftIO $ tryIO $ L.hGet (runIhdl runenv) (fromIntegral n)
		case v of
			Left _e -> return Nothing
			Right b -> runner (next b)
	CheckAuthToken _u t next -> do
		let authed = runCheckAuth runenv t
		runner (next authed)
	Relay hin hout next -> do
		v <- liftIO $ runRelay runnerio hin hout
		case v of
			Nothing -> return Nothing
			Just exitcode -> runner (next exitcode)
	RelayService service next -> do
		v <- liftIO $ runRelayService runenv runnerio service
		case v of
			Nothing -> return Nothing
			Just () -> runner next
  where
	-- This is only used for running Net actions when relaying,
	-- so it's ok to use runNetProto, despite it not supporting
	-- all Proto actions.
	runnerio = runNetProto runenv

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

runRelayService :: RunEnv -> RunProto IO -> Service -> IO (Maybe ())
runRelayService runenv runner service = bracket setup cleanup go
  where
	cmd = case service of
		UploadPack -> "upload-pack"
		ReceivePack -> "receive-pack"
	
	serviceproc = gitCreateProcess
		[ Param cmd
		, File (repoPath (runRepo runenv))
		] (runRepo runenv)

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
