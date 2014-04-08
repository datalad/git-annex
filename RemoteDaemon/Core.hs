{- git-remote-daemon core
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Core (runForeground) where

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
import Config

import Control.Concurrent.Async
import Control.Concurrent
import Network.URI
import qualified Data.Map as M

runForeground :: IO ()
runForeground = do
	(readh, writeh) <- ioHandles
	ichan <- newChan :: IO (Chan Consumed)
	ochan <- newChan :: IO (Chan Emitted)

	let reader = forever $ do
		l <- hGetLine readh
		case parseMessage l of
			Nothing -> error $ "protocol error: " ++ l
			Just cmd -> writeChan ichan cmd
	let writer = forever $ do
		msg <- readChan ochan
		hPutStrLn writeh $ unwords $ formatMessage msg
		hFlush writeh
	let controller = runController ichan ochan
	
	-- If any thread fails, the rest will be killed.
	void $ tryIO $
		reader `concurrently` writer `concurrently` controller

type RemoteMap = M.Map Git.Repo (IO (), Chan Consumed)

-- Runs the transports, dispatching messages to them, and handling
-- the main control messages.
runController :: Chan Consumed -> Chan Emitted -> IO ()
runController ichan ochan = do
	h <- genTransportHandle
	m <- genRemoteMap h ochan
	startrunning m
	go h False m
  where
	go h paused m = do
		cmd <- readChan ichan
		case cmd of
			RELOAD -> do
				liftAnnex h reloadConfig
				m' <- genRemoteMap h ochan
				let common = M.intersection m m'
				let new = M.difference m' m
				let old = M.difference m m'
				stoprunning old
				unless paused $
					startrunning new
				go h paused (M.union common new)
			PAUSE -> do
				stoprunning m
				go h True M.empty
			RESUME -> do
				when paused $
					startrunning m
				go h False m
			STOP -> exitSuccess
			-- All remaining messages are sent to
			-- all Transports.
			msg -> do
				unless paused $
					forM_ chans (`writeChan` msg)
				go h paused m
	  where
		chans = map snd (M.elems m)

	startrunning m = forM_ (M.elems m) startrunning'
	startrunning' (transport, _) = void $ async transport
	
	-- Ask the transport nicely to stop.
	stoprunning m = forM_ (M.elems m) stoprunning'
	stoprunning' (_, c) = writeChan c STOP

-- Generates a map with a transport for each supported remote in the git repo,
-- except those that have annex.sync = false
genRemoteMap :: TransportHandle -> Chan Emitted -> IO RemoteMap
genRemoteMap h@(TransportHandle g _) ochan =
	M.fromList . catMaybes <$> mapM gen (Git.remotes g)
  where
	gen r = case Git.location r of
		Git.Url u -> case M.lookup (uriScheme u) remoteTransports of
			Just transport
				| remoteAnnexSync (extractRemoteGitConfig r (Git.repoDescribe r)) -> do
					ichan <- newChan :: IO (Chan Consumed)
					return $ Just
						( r
						, (transport r (Git.repoDescribe r) h ichan ochan, ichan)
						)
			_ -> return Nothing
		_ -> return Nothing

genTransportHandle :: IO TransportHandle
genTransportHandle = do
	annexstate <- newMVar =<< Annex.new =<< Git.CurrentRepo.get
	g <- Annex.repo <$> readMVar annexstate
	return $ TransportHandle g annexstate
