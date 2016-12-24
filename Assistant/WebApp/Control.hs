{- git-annex assistant webapp control
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Control where

import Assistant.WebApp.Common
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.TransferSlots
import Assistant.Restart
import Utility.LogFile
import Utility.NotificationBroadcaster

import Control.Concurrent
import qualified Data.Map as M
import qualified Data.Text as T

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
	webapp <- getYesod
	let url = T.unpack $ yesodRender webapp (T.pack "") NotRunningR []
	{- Signal any other web browsers. -}
	liftAssistant $ do
		modifyDaemonStatus_ $ \status -> status { globalRedirUrl = Just url }
		liftIO . sendNotification . globalRedirNotifier =<< getDaemonStatus
	{- Wait 2 seconds before shutting down, to give the web
	 - page time to load in the browser. -}
	void $ liftIO $ forkIO $ do
		threadDelay 2000000
		terminateSelf
	redirect NotRunningR

{- Use a custom page to avoid putting long polling elements on it that will 
 - fail and cause thet web browser to show an error once the webapp is
 - truely stopped. -}
getNotRunningR :: Handler Html
getNotRunningR = customPage' False Nothing $
	$(widgetFile "control/notrunning")

getRestartR :: Handler Html
getRestartR = do
	liftAssistant prepRestart
	url <- liftAssistant runRestart
	liftAssistant $ postRestart url
	redirect url

getRestartThreadR :: ThreadName -> Handler ()
getRestartThreadR name = do
	m <- liftAssistant $ startedThreads <$> getDaemonStatus
	liftIO $ maybe noop snd $ M.lookup name m
	redirectBack

getLogR :: Handler Html
getLogR = page "Logs" Nothing $ do
	logfile <- liftAnnex $ fromRepo gitAnnexLogFile
	logs <- liftIO $ listLogs logfile
	logcontent <- liftIO $ concat <$> mapM readFile logs
	$(widgetFile "control/log")
