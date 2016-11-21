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
import Types.UUID
import Git
import Git.Command
import Utility.SafeCommand
import Utility.SimpleProtocol
import Utility.Exception

import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Maybe
import System.Exit (ExitCode(..))
import System.IO
import Control.Concurrent
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
		-- liftIO $ hPutStrLn stderr ("< " ++ show l)
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
		--liftIO $ hPutStrLn stderr $ "!!!" ++ show (L.length b)
		runner (next b)
	CheckAuthToken u t next -> do
		authed <- return True -- TODO XXX FIXME really check
		runner (next authed)
	Relay hout callback next ->
		runRelay runner hout callback >>= runner . next
	RelayService service callback next ->
		runRelayService s runner service callback >>= runner . next
	WriteRelay (RelayHandle h) b next -> do
		liftIO $ do
			-- L.hPut h b
			hPutStrLn h (show ("relay got:", b, L.length b))
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
		liftIO $ hPutStrLn stderr (show d)
		r <- runner $ net $ callback d
		case r of
			Nothing -> drain v
			Just exitcode -> return exitcode

runRelayService
	:: (MonadIO m, MonadMask m)
	=> S
	-> RunProto
	-> Service
	-> (RelayHandle -> RelayData -> Net (Maybe ExitCode))
	-> m ExitCode
runRelayService s runner service callback = bracket setup cleanup go
  where
	cmd = case service of
		UploadPack -> "upload-pack"
		ReceivePack -> "receive-pack"
	
	serviceproc = gitCreateProcess
		[ Param cmd
		, File (repoPath (repo s))
		] (repo s)

	setup = do
		v <- liftIO newEmptyMVar
		(Just hin, Just hout, _, pid) <- liftIO $ 
			createProcess serviceproc
				{ std_out = CreatePipe
				, std_in = CreatePipe
				}
		feeder <- liftIO $ forkIO $ feedin v
		return (v, feeder, hin, hout, pid)

	cleanup (_, feeder, hin, hout, pid) = liftIO $ do
		hClose hin
		hClose hout
		liftIO $ killThread feeder
		void $ waitForProcess pid

	go (v, _, hin, hout, pid) = do
		_ <- liftIO $ forkIO $ readout v hout
		_ <- liftIO $ forkIO $ putMVar v . Left =<< waitForProcess pid
		liftIO $ drain v hin

	drain v hin = do
		d <- takeMVar v
		case d of
			Left exitcode -> return exitcode
			Right relaydata -> do
				liftIO $ hPutStrLn stderr ("> " ++ show relaydata)
				_ <- runner $ net $
					callback (RelayHandle hin) relaydata
				drain v hin
	
	readout v hout = do
		b <- B.hGetSome hout 65536
		if B.null b
			then return ()
			else do
				putMVar v $ Right $ 
					RelayData (L.fromChunks [b])
				readout v hout

	feedin v = forever $ do
		m <- runner $ net receiveMessage
		putMVar v $ Right $ RelayMessage m
