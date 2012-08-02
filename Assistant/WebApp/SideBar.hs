{- git-annex assistant webapp sidebar
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.SideBar where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Notifications
import Assistant.DaemonStatus
import Assistant.Alert hiding (Widget)
import Utility.NotificationBroadcaster
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Map as M
import Control.Concurrent

sideBarDisplay :: Widget
sideBarDisplay = do
	let content = do
		{- Any yesod message appears as the first alert. -}
		maybe noop rendermessage =<< lift getMessage
	
		{- Add newest alerts to the sidebar. -}
		webapp <- lift getYesod
		alertpairs <- M.toList . alertMap
			<$> liftIO (getDaemonStatus $ daemonStatus webapp)
		mapM_ renderalert $
			take displayAlerts $ reverse $ sortAlertPairs alertpairs
	let ident = "sidebar"
	$(widgetFile "sidebar/main")
	autoUpdate ident NotifierSideBarR (10 :: Int) (10 :: Int)
	where
		bootstrapclass Activity = "alert-info"
		bootstrapclass Warning = "alert"
		bootstrapclass Error = "alert-error"
		bootstrapclass Success = "alert-success"
		bootstrapclass Message = "alert-info"

		renderalert (alertid, alert) = addalert
			alertid
			(alertClosable alert)
			(alertBlockDisplay alert)
			(bootstrapclass $ alertClass alert)
			(alertHeader alert)
			(alertIcon alert)
			$ case alertMessage alert of
				StringAlert s -> [whamlet|#{s}|]
				WidgetAlert w -> w alert

		rendermessage msg = addalert firstAlertId True False
			"alert-info" Nothing (Just "exclamation-sign") [whamlet|#{msg}|]

		addalert :: AlertId -> Bool -> Bool -> Text -> Maybe String -> Maybe String -> Widget -> Widget
		addalert i closable block divclass heading icon widget = do
			let alertid = show i
			let closealert = CloseAlert i
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
	webapp <- getYesod
	void $ liftIO $ removeAlert (daemonStatus webapp) i
