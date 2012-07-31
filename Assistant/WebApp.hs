{- git-annex assistant webapp data types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.WebApp where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.Alert hiding (Widget)
import Utility.NotificationBroadcaster
import Utility.WebApp
import Utility.Yesod

import Yesod
import Yesod.Static
import Text.Hamlet
import Data.Text (Text, pack, unpack)
import Control.Concurrent.STM

staticFiles "static"

mkYesodData "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

data WebApp = WebApp
	{ threadState :: ThreadState
	, daemonStatus :: DaemonStatusHandle
	, transferQueue :: TransferQueue
	, secretToken :: Text
	, relDir :: FilePath
	, getStatic :: Static
	, webAppState :: TMVar WebAppState
	}

instance Yesod WebApp where
	defaultLayout content = do
		webapp <- getYesod
		page <- widgetToPageContent $ do
			addStylesheet $ StaticR css_bootstrap_css
			addStylesheet $ StaticR css_bootstrap_responsive_css
			addScript $ StaticR jquery_full_js
			addScript $ StaticR js_bootstrap_dropdown_js
			addScript $ StaticR js_bootstrap_modal_js
			$(widgetFile "page")
		hamletToRepHtml $(hamletFile $ hamletTemplate "bootstrap")

	{- Require an auth token be set when accessing any (non-static route) -}
	isAuthorized _ _ = checkAuthToken secretToken

	{- Add the auth token to every url generated, except static subsite
         - urls (which can show up in Permission Denied pages). -}
	joinPath = insertAuthToken secretToken excludeStatic
		where
			excludeStatic [] = True
			excludeStatic (p:_) = p /= "static"

	makeSessionBackend = webAppSessionBackend
	jsLoader _ = BottomOfHeadBlocking

data WebAppState = WebAppState
	{ showIntro :: Bool
	}

newWebAppState :: IO (TMVar WebAppState)
newWebAppState = liftIO $ atomically $
	newTMVar $ WebAppState { showIntro = True }

getWebAppState :: forall sub. GHandler sub WebApp WebAppState
getWebAppState = liftIO . atomically . readTMVar =<< webAppState <$> getYesod

modifyWebAppState :: forall sub. (WebAppState -> WebAppState) -> GHandler sub WebApp ()
modifyWebAppState a = go =<< webAppState <$> getYesod
	where
		go s = liftIO $ atomically $ do
			v <- takeTMVar s
			putTMVar s $ a v

waitNotifier :: forall sub. (DaemonStatus -> NotificationBroadcaster) -> NotificationId -> GHandler sub WebApp ()
waitNotifier selector nid = do
	notifier <- getNotifier selector
	liftIO $ waitNotification $ notificationHandleFromId notifier nid

newNotifier :: forall sub. (DaemonStatus -> NotificationBroadcaster) -> GHandler sub WebApp NotificationId
newNotifier selector = do
	notifier <- getNotifier selector
	liftIO $ notificationHandleToId <$> newNotificationHandle notifier

getNotifier :: forall sub. (DaemonStatus -> NotificationBroadcaster) -> GHandler sub WebApp NotificationBroadcaster
getNotifier selector = do
	webapp <- getYesod
	liftIO $ selector <$> getDaemonStatus (daemonStatus webapp)

instance PathPiece NotificationId where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece AlertId where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack
