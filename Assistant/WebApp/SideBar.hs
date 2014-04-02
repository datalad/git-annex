{- git-annex assistant webapp sidebar
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.SideBar where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.Notifications
import Assistant.Alert.Utility
import Assistant.DaemonStatus
import Utility.NotificationBroadcaster
import Utility.Yesod

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Concurrent

sideBarDisplay :: Widget
sideBarDisplay = do
	let content = do
		{- Add newest alerts to the sidebar. -}
		alertpairs <- liftH $ M.toList . alertMap
			<$> liftAssistant getDaemonStatus
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
		let message = renderAlertMessage alert
		let messagelines = T.lines message
		let multiline = length messagelines > 1
		let buttons = zip (alertButtons alert) [1..]
		$(widgetFile "sidebar/alert")

{- Called by client to get a sidebar display.
 -
 - Returns a div, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto all pages.
 -}
getSideBarR :: NotificationId -> Handler Html
getSideBarR nid = do
	waitNotifier getAlertBroadcaster nid

	{- This 0.1 second delay avoids very transient notifications from
	 - being displayed and churning the sidebar unnecesarily. 
	 -
	 - This needs to be below the level perceptable by the user,
	 - to avoid slowing down user actions like closing alerts. -}
	liftIO $ threadDelay 100000

	page <- widgetToPageContent sideBarDisplay
	giveUrlRenderer $ [hamlet|^{pageBody page}|]

{- Called by the client to close an alert. -}
getCloseAlert :: AlertId -> Handler ()
getCloseAlert = liftAssistant . removeAlert

{- When an alert with a button is clicked on, the button takes us here. -}
getClickAlert :: AlertId -> Int -> Handler ()
getClickAlert i bnum = do
	m <- alertMap <$> liftAssistant getDaemonStatus
	case M.lookup i m of
		Just (Alert { alertButtons = bs })
			| length bs >= bnum -> do
				let b = bs !! (bnum - 1)
				{- Spawn a thread to run the action
				 - while redirecting. -}
				case buttonAction b of
					Nothing -> noop
					Just a -> liftIO $ void $ forkIO $ a i
				redirect $ buttonUrl b
			| otherwise -> redirectBack
		_ -> redirectBack

htmlIcon :: AlertIcon -> Widget
htmlIcon ActivityIcon = [whamlet|<img src="@{StaticR activityicon_gif}" alt="">|]
htmlIcon SyncIcon = [whamlet|<img src="@{StaticR syncicon_gif}" alt="">|]
htmlIcon InfoIcon = bootstrapIcon "info-sign"
htmlIcon SuccessIcon = bootstrapIcon "ok"
htmlIcon ErrorIcon = bootstrapIcon "exclamation-sign"
htmlIcon UpgradeIcon = bootstrapIcon "arrow-up"
-- utf-8 umbrella (utf-8 cloud looks too stormy)
htmlIcon TheCloud = [whamlet|&#9730;|]

bootstrapIcon :: Text -> Widget
bootstrapIcon name = [whamlet|<i .icon-#{name}></i>|]
