{- git-annex assistant communication with remotedaemon
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.RemoteControl where

import Assistant.Common
import RemoteDaemon.Types
import Config.Files
import Utility.Batch
import Utility.SimpleProtocol
import Assistant.Alert
import Assistant.Alert.Utility
import Assistant.DaemonStatus

import Control.Concurrent
import Control.Concurrent.Async
import System.Process (std_in, std_out)
import qualified Data.Map as M

remoteControlThread :: NamedThread
remoteControlThread = namedThread "RemoteControl" $ do
	program <- liftIO readProgramFile
	(cmd, params) <- liftIO $ toBatchCommand
		(program, [Param "remotedaemon"])
	let p = proc cmd (toCommand params)
	(Just toh, Just fromh, _, pid) <- liftIO $ createProcess p
		{ std_in = CreatePipe
		, std_out = CreatePipe
		}
	
	controller <- asIO $ remoteControllerThread toh
	responder <- asIO $ remoteResponderThread fromh

	-- run controller and responder until the remotedaemon dies
	liftIO $ do
		void $ controller `concurrently` responder
		forceSuccessProcess p pid

-- feed from the remoteControl channel into the remotedaemon
remoteControllerThread :: Handle -> Assistant ()
remoteControllerThread toh = do
	clicker <- getAssistant remoteControl
	liftIO $ forever $ do
		msg <- readChan clicker
		hPutStrLn toh $ unwords $ formatMessage msg
		hFlush toh

-- read status messages emitted by the remotedaemon and handle them
remoteResponderThread :: Handle -> Assistant ()
remoteResponderThread fromh = go M.empty
  where
	go syncalerts = do
		l <- liftIO $ hGetLine fromh
		case parseMessage l of
			Just (CONNECTED _rn) -> do
				go syncalerts
			Just (DISCONNECTED _rn) -> do
				go syncalerts
			Just (SYNCING rn)
				| M.member rn syncalerts -> go syncalerts
				| otherwise -> do
					i <- addAlert $ syncAlert' [rn]
					go (M.insert rn i syncalerts)
			Just (DONESYNCING status rn) ->
				case M.lookup rn syncalerts of
					Nothing -> go syncalerts
					Just i -> do
						let (succeeded, failed) = if status
							then ([rn], [])
							else ([], [rn])
						updateAlertMap $ mergeAlert i $
							syncResultAlert' succeeded failed
						go (M.delete rn syncalerts)
			Nothing -> do
				debug ["protocol error from remotedaemon: ", l]
				go syncalerts
