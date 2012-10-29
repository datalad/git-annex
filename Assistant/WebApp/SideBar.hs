{- git-annex assistant webapp sidebar
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.SideBar where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.Notifications
import Assistant.DaemonStatus
import Assistant.Alert
import Utility.NotificationBroadcaster
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Map as M
import Control.Concurrent

sideBarDisplay :: Widget
sideBarDisplay = do
	let content = do
		{- Add newest alerts to the sidebar. -}
		alertpairs <- lift $ M.toList . alertMap <$> getDaemonStatusY
		mapM_ renderalert $
			take displayAlerts $ reverse $ sortAlertPairs alertpairs
	let ident = "sidebar"
	$(widgetFile "sidebar/main")
	autoUpdate ident NotifierSideBarR (10 :: Int) (10 :: Int)
	where
		bootstrapclass :: AlertClass -> Text
		bootstrapclass Activity = "alert-info"
		bootstrapclass Warning = "alert"
		bootstrapclass Error = "alert-error"
		bootstrapclass Success = "alert-success"
		bootstrapclass Message = "alert-info"

		renderalert (aid, alert) = do
			let alertid = show aid
			let closable = alertClosable alert
			let block = alertBlockDisplay alert
			let divclass = bootstrapclass $ alertClass alert
			$(widgetFile "sidebar/alert")

{- Called by client to get a sidebar display.
 -
 - Returns a div, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto all pages.
 -}
getSideBarR :: NotificationId -> Handler RepHtml
getSideBarR nid = do
	waitNotifier alertNotifier nid

	{- This 0.1 second delay avoids very transient notifications from
	 - being displayed and churning the sidebar unnecesarily. 
	 -
	 - This needs to be below the level perceptable by the user,
	 - to avoid slowing down user actions like closing alerts. -}
	liftIO $ threadDelay 100000

	page <- widgetToPageContent sideBarDisplay
	hamletToRepHtml $ [hamlet|^{pageBody page}|]

{- Called by the client to close an alert. -}
getCloseAlert :: AlertId -> Handler ()
getCloseAlert i = do
	dstatus <- getAssistantY daemonStatus
	liftIO $ removeAlert dstatus i

{- When an alert with a button is clicked on, the button takes us here. -}
getClickAlert :: AlertId -> Handler ()
getClickAlert i = do
	m <- alertMap <$> getDaemonStatusY
	case M.lookup i m of
		Just (Alert { alertButton = Just b }) -> do
			{- Spawn a thread to run the action while redirecting. -}
			case buttonAction b of
				Nothing -> noop
				Just a -> liftIO $ void $ forkIO $ a i
			redirect $ buttonUrl b
		_ -> redirectBack

