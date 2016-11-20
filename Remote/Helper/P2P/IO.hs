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

import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Maybe
import System.Exit (ExitCode(..))
import System.IO
import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type RunProto = forall a m. MonadIO m => Proto a -> m a

data S = S
	{ repo :: Repo
	, hdl :: Handle
	}

-- Implementation of the protocol, communicating with a peer
-- over a Handle. No Local actions will be run.
runNetProtoHandle :: MonadIO m => Handle -> Repo -> Proto a -> m a
runNetProtoHandle h r = go
  where
	go :: RunProto
	go (Pure a) = pure a
	go (Free (Net n)) = runNetHandle (S r h) go n
	go (Free (Local _)) = error "local actions not allowed"

runNetHandle :: MonadIO m => S -> RunProto -> NetF (Proto a) -> m a
runNetHandle s runner f = case f of
	SendMessage m next -> do
		liftIO $ do
			hPutStrLn (hdl s) (unwords (formatMessage m))
			hFlush (hdl s)
		runner next
	ReceiveMessage next -> do
		l <- liftIO $ hGetLine (hdl s)
		let m = fromMaybe (ERROR "protocol parse error")
			(parseMessage l)
		runner (next m)
	SendBytes _len b next -> do
		liftIO $ do
			L.hPut (hdl s) b
			hFlush (hdl s)
		runner next
	ReceiveBytes (Len n) next -> do
		b <- liftIO $ L.hGet (hdl s) (fromIntegral n)
		runner (next b)
	Relay hout callback next ->
		runRelay runner hout callback >>= runner . next
	RelayService service callback next ->
		runRelayService s runner service callback >>= runner . next
	WriteRelay (RelayHandle h) b next -> do
		liftIO $ do
			L.hPut h b
			hFlush h
		runner next

runRelay
	:: MonadIO m
	=> RunProto
	-> RelayHandle
	-> (RelayData -> Net (Maybe ExitCode))
	-> m ExitCode
runRelay runner (RelayHandle hout) callback = do
	v <- liftIO newEmptyMVar
	_ <- liftIO $ forkIO $ readout v
	feeder <- liftIO $ forkIO $ feedin v
	exitcode <- liftIO $ drain v
	liftIO $ killThread feeder
	return exitcode
  where
	feedin v = forever $ do
		m <- runner $ net receiveMessage
		putMVar v $ RelayMessage m
	
	readout v = do
		b <- B.hGetSome hout 65536
		if B.null b
			then hClose hout
			else do
				putMVar v $ RelayData (L.fromChunks [b])
				readout v

	drain v = do
		d <- takeMVar v
		r <- runner $ net $ callback d
		case r of
			Nothing -> drain v
			Just exitcode -> return exitcode

runRelayService
	:: MonadIO m
	=> S
	-> RunProto
	-> Service
	-> (RelayHandle -> RelayData -> Net (Maybe ExitCode))
	-> m ExitCode
runRelayService s runner service callback = do
	v <- liftIO newEmptyMVar
	(Just hin, Just hout, _, pid) <- liftIO $ createProcess serviceproc
		{ std_out = CreatePipe
		, std_in = CreatePipe
		}
	_ <- liftIO $ forkIO $ readout v hout
	feeder <- liftIO $ forkIO $ feedin v
	_ <- liftIO $ forkIO $ putMVar v . Left =<< waitForProcess pid
	exitcode <- liftIO $ drain v hin
	liftIO $ killThread feeder
	return exitcode
  where
	cmd = case service of
		UploadPack -> "upload-pack"
		ReceivePack -> "receive-pack"
	serviceproc = gitCreateProcess [Param cmd, File (repoPath (repo s))] (repo s)

	drain v hin = do
		d <- takeMVar v
		case d of
			Left exitcode -> do
				hClose hin
				return exitcode
			Right relaydata -> do
				_ <- runner $ net $
					callback (RelayHandle hin) relaydata
				drain v hin
	
	readout v hout = do
		b <- B.hGetSome hout 65536
		if B.null b
			then hClose hout
			else do
				putMVar v $ Right $ 
					RelayData (L.fromChunks [b])
				readout v hout

	feedin v = forever $ do
		m <- runner $ net receiveMessage
		putMVar v $ Right $ RelayMessage m
