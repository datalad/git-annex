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
import RemoteDaemon.Types
import RemoteDaemon.Transport
import qualified Git
import qualified Git.Types as Git
import qualified Git.CurrentRepo
import Utility.SimpleProtocol

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Network.URI
import qualified Data.Map as M

runForeground :: IO ()
runForeground = do
	ichan <- newChan :: IO (Chan Consumed)
	ochan <- newChan :: IO (Chan Emitted)

	void $ async $ controller ichan ochan

	let reader = forever $ do
		l <- getLine
		case parseMessage l of
			Nothing -> error $ "protocol error: " ++ l
			Just cmd -> writeChan ichan cmd
	let writer = forever $ do
		msg <- readChan ochan
		putStrLn $ unwords $ formatMessage msg
		hFlush stdout
	
	-- If the reader or writer fails, for example because stdin/stdout
	-- gets closed, kill the other one, and throw an exception which
	-- will take down the daemon.
	void $ concurrently reader writer

type RemoteMap = M.Map Git.Repo (IO (), Chan Consumed)

-- Runs the transports, dispatching messages to them, and handling
-- the main control messages.
controller :: Chan Consumed -> Chan Emitted -> IO ()
controller ichan ochan = do
	m <- getRemoteMap ochan
	startrunning m
	go False m
  where
	go paused m = do
		cmd <- readChan ichan
		case cmd of
			RELOAD -> do
				m' <- getRemoteMap ochan
				let common = M.intersection m m'
				let new = M.difference m' m
				let old = M.difference m m'
				stoprunning old
				unless paused $
					startrunning new
				go paused (M.union common new)
			PAUSE -> do
				stoprunning m
				go True m
			RESUME -> do
				when paused $
					startrunning m
				go False m
			STOP -> exitSuccess
			-- All remaining messages are sent to
			-- all Transports.
			msg -> do
				unless paused $
					forM_ chans (`writeChan` msg)
				go paused m
	  where
		chans = map snd (M.elems m)

	startrunning m = forM_ (M.elems m) startrunning'
	startrunning' (transport, _) = void $ async transport
	
	-- Ask the transport nicely to stop.
	stoprunning m = forM_ (M.elems m) stoprunning'
	stoprunning' (_, c) = writeChan c STOP

getRemoteMap :: Chan Emitted -> IO RemoteMap
getRemoteMap ochan = do
	annexstate <- Annex.new =<< Git.CurrentRepo.get
	genRemoteMap annexstate ochan

-- Generates a map with a transport for each supported remote in the git repo,
-- except those that have annex.sync = false
genRemoteMap :: Annex.AnnexState -> Chan Emitted -> IO RemoteMap
genRemoteMap annexstate ochan = M.fromList . catMaybes <$> mapM gen rs
  where
	rs = Git.remotes (Annex.repo annexstate)
	gen r = case Git.location r of
		Git.Url u -> case M.lookup (uriScheme u) remoteTransports of
			Just transport
				| remoteAnnexSync (extractRemoteGitConfig r (Git.repoDescribe r)) -> do
					ichan <- newChan :: IO (Chan Consumed)
					return $ Just
						( r
						, (transport r (Git.repoDescribe r) annexstate ichan ochan, ichan)
						)
			_ -> return Nothing
		_ -> return Nothing
