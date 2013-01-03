{- git-annex assistant webapp control
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Control where

import Assistant.WebApp.Common

import Control.Concurrent
import System.Posix (getProcessID, signalProcess, sigTERM)

getShutdownR :: Handler RepHtml
getShutdownR = page "Shutdown" Nothing $
	$(widgetFile "control/shutdown")

getShutdownConfirmedR :: Handler RepHtml
getShutdownConfirmedR = page "Shutdown" Nothing $ do
	{- Wait 2 seconds before shutting down, to give the web page time
	 - to display. -}
	void $ liftIO $ forkIO $ do
		threadDelay 2000000
		signalProcess sigTERM =<< getProcessID
	$(widgetFile "control/shutdownconfirmed")
