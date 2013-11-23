{- git-annex assistant webapp notifications
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

#if defined VERSION_yesod_default
#if ! MIN_VERSION_yesod_default(1,1,0)
#define WITH_OLD_YESOD
#endif
#endif

module Assistant.WebApp.Notifications where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.DaemonStatus
import Assistant.Types.Buddies
import Utility.NotificationBroadcaster
import Utility.Yesod

import Data.Text (Text)
import qualified Data.Text as T
#ifndef WITH_OLD_YESOD
import qualified Data.Aeson.Types as Aeson
#endif
import Control.Concurrent

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
#ifdef WITH_OLD_YESOD
	let delay = show ms_delay
	let startdelay = show ms_startdelay
	let ident = "'" ++ T.unpack tident ++ "'"
#else
	let delay = Aeson.String (T.pack (show ms_delay))
	let startdelay = Aeson.String (T.pack (show ms_startdelay))
	let ident = Aeson.String tident
#endif
	addScript $ StaticR longpolling_js
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
		, secretToken webapp
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

getGlobalRedirR :: NotificationId -> Handler Text
getGlobalRedirR nid = do
	tid <- liftIO myThreadId
	liftIO $ do
		hPutStrLn stderr $ show ("getGlobalRedirR waiting", tid)
		hFlush stderr
	waitNotifier getGlobalRedirBroadcaster nid
	v <- globalRedirUrl <$> liftAssistant getDaemonStatus
	liftIO $ do
		hPutStrLn stderr $ show ("getGlobalRedirR got a val", v, tid)
		hFlush stderr
	maybe (getGlobalRedirR nid) (return . T.pack)
		=<< globalRedirUrl <$> liftAssistant getDaemonStatus
