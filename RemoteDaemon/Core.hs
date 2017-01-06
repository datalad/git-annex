{- git-remote-daemon core
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Core (runInteractive, runNonInteractive) where

import qualified Annex
import Common
import Types.GitConfig
import RemoteDaemon.Common
import RemoteDaemon.Types
import RemoteDaemon.Transport
import qualified Git
import qualified Git.Types as Git
import qualified Git.CurrentRepo
import Utility.SimpleProtocol
import Utility.ThreadScheduler
import Config
import Annex.Ssh
import Types.Messages

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.URI
import qualified Data.Map as M

runInteractive :: IO ()
runInteractive = do
	(readh, writeh) <- dupIoHandles
	ichan <- newTChanIO :: IO (TChan Consumed)
	ochan <- newTChanIO :: IO (TChan Emitted)

	let reader = forever $ do
		l <- hGetLine readh
		case parseMessage l of
			Nothing -> error $ "protocol error: " ++ l
			Just cmd -> atomically $ writeTChan ichan cmd
	let writer = forever $ do
		msg <- atomically $ readTChan ochan
		hPutStrLn writeh $ unwords $ formatMessage msg
		hFlush writeh
	let controller = runController ichan ochan
	
	-- If any thread fails, the rest will be killed.
	void $ tryIO $ reader
		`concurrently` writer
		`concurrently` controller

runNonInteractive :: IO ()
runNonInteractive = do
	ichan <- newTChanIO :: IO (TChan Consumed)
	ochan <- newTChanIO :: IO (TChan Emitted)
	
	let reader = forever $ do
		threadDelaySeconds (Seconds (60*60))
		atomically $ writeTChan ichan RELOAD
	let writer = forever $
		void $ atomically $ readTChan ochan
	let controller = runController ichan ochan
	
	void $ tryIO $ reader
		`concurrently` writer
		`concurrently` controller

type RemoteMap = M.Map Git.Repo (IO (), TChan Consumed)

-- Runs the transports, dispatching messages to them, and handling
-- the main control messages.
runController :: TChan Consumed -> TChan Emitted -> IO ()
runController ichan ochan = do
	h <- genTransportHandle
	m <- genRemoteMap h ochan
	starttransports m
	serverchans <- mapM (startserver h) remoteServers
	go h False m serverchans
  where
	go h paused m serverchans = do
		cmd <- atomically $ readTChan ichan
		broadcast cmd serverchans
		case cmd of
			RELOAD -> do
				h' <- updateTransportHandle h
				m' <- genRemoteMap h' ochan
				let common = M.intersection m m'
				let new = M.difference m' m
				let old = M.difference m m'
				broadcast STOP (mchans old)
				unless paused $
					starttransports new
				go h' paused (M.union common new) serverchans
			LOSTNET -> do
				-- force close all cached ssh connections
				-- (done here so that if there are multiple
				-- ssh remotes, it's only done once)
				liftAnnex h forceSshCleanup
				broadcast LOSTNET transportchans
				go h True m serverchans
			PAUSE -> do
				broadcast STOP transportchans
				go h True m serverchans
			RESUME -> do
				when paused $
					starttransports m
				go h False m serverchans
			STOP -> exitSuccess
			-- All remaining messages are sent to
			-- all Transports.
			msg -> do
				unless paused $
					broadcast msg transportchans
				go h paused m serverchans
	  where
		transportchans = mchans m
		mchans = map snd . M.elems
	
	startserver h server = do
		c <- newTChanIO
		void $ async $ server c h
		return c

	starttransports m = forM_ (M.elems m) starttransports'
	starttransports' (transport, c) = do
		-- drain any old control messages from the channel
		-- to avoid confusing the transport with them
		atomically $ drain c
		void $ async transport
	
	drain c = maybe noop (const $ drain c) =<< tryReadTChan c
	
	broadcast msg cs = atomically $ forM_ cs $ \c -> writeTChan c msg

-- Generates a map with a transport for each supported remote in the git repo,
-- except those that have annex.sync = false
genRemoteMap :: TransportHandle -> TChan Emitted -> IO RemoteMap
genRemoteMap h@(TransportHandle (LocalRepo g) _) ochan = 
	M.fromList . catMaybes <$> mapM gen (Git.remotes g)
  where
	gen r = case Git.location r of
		Git.Url u -> case M.lookup (uriScheme u) remoteTransports of
			Just transport
				| remoteAnnexSync gc -> do
					ichan <- newTChanIO :: IO (TChan Consumed)
					return $ Just
						( r
						, (transport (RemoteRepo r gc) (RemoteURI u) h ichan ochan, ichan)
						)
			_ -> return Nothing
		_ -> return Nothing
	  where
		gc = extractRemoteGitConfig g (Git.repoDescribe r)

genTransportHandle :: IO TransportHandle
genTransportHandle = do
	annexstate <- newMVar =<< Annex.new =<< Git.CurrentRepo.get
	g <- Annex.repo <$> readMVar annexstate
	let h = TransportHandle (LocalRepo g) annexstate
	liftAnnex h $ Annex.setOutput QuietOutput
	return h

updateTransportHandle :: TransportHandle -> IO TransportHandle
updateTransportHandle h@(TransportHandle _g annexstate) = do
	g' <- liftAnnex h $ do
		reloadConfig
		Annex.fromRepo id
	return (TransportHandle (LocalRepo g') annexstate)
