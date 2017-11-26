{- git-annex assistant communication with remotedaemon
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.RemoteControl where

import Assistant.Common
import RemoteDaemon.Types
import Annex.Path
import Utility.Batch
import Utility.SimpleProtocol
import Assistant.Alert
import Assistant.Alert.Utility
import Assistant.DaemonStatus
import qualified Git
import qualified Git.Types as Git
import qualified Remote
import qualified Types.Remote as Remote

import Control.Concurrent
import Control.Concurrent.Async
import Network.URI
import qualified Data.Map as M
import qualified Data.Set as S

remoteControlThread :: NamedThread
remoteControlThread = namedThread "RemoteControl" $ do
	program <- liftIO programPath
	(cmd, params) <- liftIO $ toBatchCommand
		(program, [Param "remotedaemon", Param "--foreground"])
	let p = proc cmd (toCommand params)
	(Just toh, Just fromh, _, pid) <- liftIO $ createProcess p
		{ std_in = CreatePipe
		, std_out = CreatePipe
		}
	
	urimap <- liftIO . newMVar =<< liftAnnex getURIMap

	controller <- asIO $ remoteControllerThread toh
	responder <- asIO $ remoteResponderThread fromh urimap

	-- run controller and responder until the remotedaemon dies
	liftIO $ void $ tryNonAsync $ controller `concurrently` responder
	debug ["remotedaemon exited"]
	liftIO $ forceSuccessProcess p pid

-- feed from the remoteControl channel into the remotedaemon
remoteControllerThread :: Handle -> Assistant ()
remoteControllerThread toh = do
	clicker <- getAssistant remoteControl
	forever $ do
		msg <- liftIO $ readChan clicker
		debug [show msg]
		liftIO $ do
			hPutStrLn toh $ unwords $ formatMessage msg
			hFlush toh

-- read status messages emitted by the remotedaemon and handle them
remoteResponderThread :: Handle -> MVar (M.Map URI Remote) -> Assistant ()
remoteResponderThread fromh urimap = go M.empty
  where
	go syncalerts = do
		l <- liftIO $ hGetLine fromh
		debug [l]
		case parseMessage l of
			Just (CONNECTED uri) -> changeconnected S.insert uri
			Just (DISCONNECTED uri) -> changeconnected S.delete uri
			Just (SYNCING uri) -> withr uri $ \r ->
				if M.member (Remote.uuid r) syncalerts
					then go syncalerts
					else do
						i <- addAlert $ syncAlert [r]
						go (M.insert (Remote.uuid r) i syncalerts)
			Just (DONESYNCING uri status) -> withr uri $ \r ->
				case M.lookup (Remote.uuid r) syncalerts of
					Nothing -> cont
					Just i -> do
						let (succeeded, failed) = if status
							then ([r], [])
							else ([], [r])
						updateAlertMap $ mergeAlert i $
							syncResultAlert succeeded failed
						go (M.delete (Remote.uuid r) syncalerts)
			Just (WARNING (RemoteURI uri) msg) -> do
				void $ addAlert $
					warningAlert ("RemoteControl "++ show uri) msg
				cont
			Nothing -> do
				debug ["protocol error from remotedaemon: ", l]
				cont
	  where
		cont = go syncalerts
		withr uri = withRemote uri urimap cont
		changeconnected sm uri = withr uri $ \r -> do
			changeCurrentlyConnected $ sm $ Remote.uuid r
			cont

getURIMap :: Annex (M.Map URI Remote)
getURIMap = Remote.remoteMap' id (mkk . Git.location . Remote.repo)
  where
	mkk (Git.Url u) = Just u
	mkk _ = Nothing

withRemote
	:: RemoteURI
	-> MVar (M.Map URI Remote)
	-> Assistant a
	-> (Remote -> Assistant a)
	-> Assistant a
withRemote (RemoteURI uri) remotemap noremote a = do
	m <- liftIO $ readMVar remotemap
	case M.lookup uri m of
		Just r -> a r
		Nothing -> do
			{- Reload map, in case a new remote has been added. -}
			m' <- liftAnnex getURIMap
			void $ liftIO $ swapMVar remotemap $ m'
			maybe noremote a (M.lookup uri m')
