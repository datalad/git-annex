{- git-annex assistant webapp control
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Control where

import Assistant.WebApp.Common
import Config.Files
import Utility.LogFile
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.TransferSlots

import Control.Concurrent
import System.Posix (getProcessID, signalProcess, sigTERM)
import qualified Data.Map as M

getShutdownR :: Handler Html
getShutdownR = page "Shutdown" Nothing $
	$(widgetFile "control/shutdown")

getShutdownConfirmedR :: Handler Html
getShutdownConfirmedR = do
	liftAssistant $ do
		{- Remove all alerts for currently running activities. -}
		updateAlertMap $ M.filter $ \a -> alertClass a /= Activity
		void $ addAlert shutdownAlert
		{- Stop transfers the assistant is running,
		 - otherwise they would continue past shutdown.
		 - Pausing transfers prevents more being started up (and stops
		 - the transfer processes). -}
		ts <- M.keys . currentTransfers <$> getDaemonStatus
		mapM_ pauseTransfer ts
	page "Shutdown" Nothing $ do
		{- Wait 2 seconds before shutting down, to give the web
		 - page time to load in the browser. -}
		void $ liftIO $ forkIO $ do
			threadDelay 2000000
			signalProcess sigTERM =<< getProcessID
		$(widgetFile "control/shutdownconfirmed")

{- Quite a hack, and doesn't redirect the browser window. -}
getRestartR :: Handler Html
getRestartR = page "Restarting" Nothing $ do
	void $ liftIO $ forkIO $ do
		threadDelay 2000000
		program <- readProgramFile
		unlessM (boolSystem "sh" [Param "-c", Param $ restartcommand program]) $
			error "restart failed"
	$(widgetFile "control/restarting")
  where
	restartcommand program = program ++ " assistant --stop; exec " ++
		program ++ " webapp"

getRestartThreadR :: ThreadName -> Handler ()
getRestartThreadR name = do
	m <- liftAssistant $ startedThreads <$> getDaemonStatus
	liftIO $ maybe noop snd $ M.lookup name m
	redirectBack

getLogR :: Handler Html
getLogR = page "Logs" Nothing $ do
	logfile <- liftAnnex $ fromRepo gitAnnexLogFile
	logs <- liftIO $ listLogs logfile
	logcontent <- liftIO $ concat <$> mapM readFileStrictAnyEncoding logs
	$(widgetFile "control/log")
