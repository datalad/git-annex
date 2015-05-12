{- git-annex assistant webapp notifications
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Notifications where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.DaemonStatus
import Assistant.Types.Buddies
import Utility.NotificationBroadcaster
import Utility.Yesod
import Utility.WebApp

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson.Types as Aeson

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
autoUpdate tident geturl ms_delay ms_startdelay = do
	let delay = Aeson.String (T.pack (show ms_delay))
	let startdelay = Aeson.String (T.pack (show ms_startdelay))
	let ident = Aeson.String tident
	$(widgetFile "notifications/longpolling")

{- Notifier urls are requested by the javascript, to avoid allocation
 - of NotificationIds when noscript pages are loaded. This constructs a
 - notifier url for a given Route and NotificationBroadcaster.
 -}
notifierUrl :: (NotificationId -> Route WebApp) -> Assistant NotificationBroadcaster -> Handler RepPlain
notifierUrl route broadcaster = do
	(urlbits, _params) <- renderRoute . route <$> newNotifier broadcaster
	webapp <- getYesod
	return $ RepPlain $ toContent $ T.concat
		[ "/"
		, T.intercalate "/" urlbits
		, "?auth="
		, fromAuthToken (authToken webapp)
		]

getNotifierTransfersR :: Handler RepPlain
getNotifierTransfersR = notifierUrl TransfersR getTransferBroadcaster

getNotifierSideBarR :: Handler RepPlain
getNotifierSideBarR = notifierUrl SideBarR getAlertBroadcaster

getNotifierBuddyListR :: Handler RepPlain
getNotifierBuddyListR = notifierUrl BuddyListR getBuddyListBroadcaster

getNotifierRepoListR :: RepoSelector -> Handler RepPlain
getNotifierRepoListR reposelector = notifierUrl route getRepoListBroadcaster
  where
	route nid = RepoListR nid reposelector

getNotifierGlobalRedirR :: Handler RepPlain
getNotifierGlobalRedirR = notifierUrl GlobalRedirR getGlobalRedirBroadcaster

getTransferBroadcaster :: Assistant NotificationBroadcaster
getTransferBroadcaster = transferNotifier <$> getDaemonStatus

getAlertBroadcaster :: Assistant NotificationBroadcaster
getAlertBroadcaster = alertNotifier <$> getDaemonStatus

getBuddyListBroadcaster :: Assistant NotificationBroadcaster
getBuddyListBroadcaster =  getBuddyBroadcaster <$> getAssistant buddyList

getRepoListBroadcaster :: Assistant NotificationBroadcaster
getRepoListBroadcaster =  syncRemotesNotifier <$> getDaemonStatus

getGlobalRedirBroadcaster :: Assistant NotificationBroadcaster
getGlobalRedirBroadcaster =  globalRedirNotifier <$> getDaemonStatus

getGlobalRedirR :: NotificationId -> Handler RepPlain
getGlobalRedirR nid = do
	waitNotifier getGlobalRedirBroadcaster nid
	maybe (getGlobalRedirR nid) (return . RepPlain . toContent . T.pack)
		=<< globalRedirUrl <$> liftAssistant getDaemonStatus
