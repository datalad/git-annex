{- P2P protocol, partial IO implementation
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Helper.P2P.IO
	( RunProto
	, runNetProtoHandle
	) where

import Remote.Helper.P2P
import Utility.Process
import Git
import Git.Command
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

type RunProto = forall a m. (MonadIO m, MonadMask m) => Proto a -> m a

data S = S
	{ repo :: Repo
	, ihdl :: Handle
	, ohdl :: Handle
	}

-- Implementation of the protocol, communicating with a peer
-- over a Handle. No Local actions will be run.
runNetProtoHandle :: (MonadIO m, MonadMask m) => Handle -> Handle -> Repo -> Proto a -> m a
runNetProtoHandle i o r = go
  where
	go :: RunProto
	go (Pure a) = pure a
	go (Free (Net n)) = runNetHandle (S r i o) go n
	go (Free (Local _)) = error "local actions not allowed"

runNetHandle :: (MonadIO m, MonadMask m) => S -> RunProto -> NetF (Proto a) -> m a
runNetHandle s runner f = case f of
	SendMessage m next -> do
		liftIO $ do
			hPutStrLn (ohdl s) (unwords (formatMessage m))
			hFlush (ohdl s)
		runner next
	ReceiveMessage next -> do
		l <- liftIO $ hGetLine (ihdl s)
		case parseMessage l of
			Just m -> runner (next m)
			Nothing -> runner $ do
				let e = ERROR $ "protocol parse error: " ++ show l
				net $ sendMessage e
				next e
	SendBytes _len b next -> do
		liftIO $ do
			L.hPut (ohdl s) b
			hFlush (ohdl s)
		runner next
	ReceiveBytes (Len n) next -> do
		b <- liftIO $ L.hGet (ihdl s) (fromIntegral n)
		runner (next b)
	CheckAuthToken u t next -> do
		authed <- return True -- TODO XXX FIXME really check
		runner (next authed)
	Relay hin hout next ->
		runRelay runner hin hout >>= runner . next
	RelayService service next ->
		runRelayService s runner service >> runner next

runRelay
	:: MonadIO m
	=> RunProto
	-> RelayHandle
	-> RelayHandle
	-> m ExitCode
runRelay runner (RelayHandle hout) (RelayHandle hin) = liftIO $
	bracket setup cleanup go
  where
	setup = do
		v <- newEmptyMVar
		void $ forkIO $ relayFeeder runner v
		void $ forkIO $ relayReader v hout
		return v
	
	cleanup _ = do
		hClose hin
		hClose hout
	
	go v = relayHelper runner v hin

runRelayService
	:: MonadIO m
	=> S
	-> RunProto
	-> Service
	-> m ()
runRelayService s runner service = liftIO $ bracket setup cleanup go
  where
	cmd = case service of
		UploadPack -> "upload-pack"
		ReceivePack -> "receive-pack"
	
	serviceproc = gitCreateProcess
		[ Param cmd
		, File (repoPath (repo s))
		] (repo s)

	setup = do
		(Just hin, Just hout, _, pid) <- createProcess serviceproc
			{ std_out = CreatePipe
			, std_in = CreatePipe
			}
		v <- newEmptyMVar
		feeder <- async $ relayFeeder runner v
		reader <- async $ relayReader v hout
		waiter <- async $ waitexit v pid
		return (v, feeder, reader, waiter, hin, hout, pid)

	cleanup (_, feeder, reader, waiter, hin, hout, pid) = do
		hPutStrLn stderr "!!!!\n\nIN CLEANUP"
		hFlush stderr
		hClose hin
		hClose hout
		cancel reader
		cancel waiter
		void $ waitForProcess pid

	go (v, _, _, _, hin, _, _) = do
		exitcode <- relayHelper runner v hin
		runner $ net $ relayToPeer (RelayDone exitcode)
	
	waitexit v pid = putMVar v . RelayDone =<< waitForProcess pid

-- Processes RelayData as it is put into the MVar.
relayHelper :: RunProto -> MVar RelayData -> Handle -> IO ExitCode
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
				runner $ net $ relayToPeer (RelayToPeer b)
				loop
			RelayDone exitcode -> do
				runner $ net $ relayToPeer (RelayDone exitcode)
				return exitcode

-- Takes input from the peer, and puts it into the MVar for processing.
-- Repeats until the peer tells it it's done.
relayFeeder :: RunProto -> MVar RelayData -> IO ()
relayFeeder runner v = loop
  where
	loop = do
		rd <- runner $ net relayFromPeer
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
		b <- B.hGetSome hout 65536
		if B.null b
			then return ()
			else do
				putMVar v $ RelayToPeer (L.fromChunks [b])
				loop
