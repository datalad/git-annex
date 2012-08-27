{- git-annex assistant webapp notifications
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Notifications where

import Assistant.Common
import Assistant.WebApp
import Assistant.DaemonStatus
import Utility.NotificationBroadcaster
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Text as T

{- Add to any widget to make it auto-update using long polling.
 -
 - The widget should have a html element with an id=ident, which will be
 - replaced when it's updated.
 -
 - The geturl route should return the notifier url to use for polling.
 -
 - ms_delay is how long to delay between AJAX updates
 - ms_startdelay is how long to delay before updating with AJAX at the start
 -}
autoUpdate :: Text -> Route WebApp -> Int -> Int -> Widget
autoUpdate ident geturl ms_delay ms_startdelay = do
	let delay = show ms_delay
	let startdelay = show ms_startdelay
	addScript $ StaticR longpolling_js
	$(widgetFile "notifications/longpolling")

{- Notifier urls are requested by the javascript, to avoid allocation
 - of NotificationIds when noscript pages are loaded. This constructs a
 - notifier url for a given Route and NotificationBroadcaster.
 -}
notifierUrl :: (NotificationId -> Route WebApp) -> (DaemonStatus -> NotificationBroadcaster) -> Handler RepPlain
notifierUrl route selector = do
	(urlbits, _params) <- renderRoute . route <$> newNotifier selector
	webapp <- getYesod
	return $ RepPlain $ toContent $ T.concat
		[ "/"
		, T.intercalate "/" urlbits
		, "?auth="
		, secretToken webapp
		]

getNotifierTransfersR :: Handler RepPlain
getNotifierTransfersR = notifierUrl TransfersR transferNotifier

getNotifierSideBarR :: Handler RepPlain
getNotifierSideBarR = notifierUrl SideBarR alertNotifier
