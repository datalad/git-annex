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
	{ threadState :: Maybe ThreadState
	, daemonStatus :: DaemonStatusHandle
	, transferQueue :: TransferQueue
	, secretToken :: Text
	, relDir :: Maybe FilePath
	, getStatic :: Static
	, webAppState :: TMVar WebAppState
	, postFirstRun :: Maybe (IO String)
	}

data NavBarItem = DashBoard | Config | About
	deriving (Eq)

navBarName :: NavBarItem -> Text
navBarName DashBoard = "Dashboard"
navBarName Config = "Configuration"
navBarName About = "About"

navBarRoute :: NavBarItem -> Route WebApp
navBarRoute DashBoard = HomeR
navBarRoute Config = ConfigR
navBarRoute About = AboutR

defaultNavBar :: [NavBarItem]
defaultNavBar = [DashBoard, Config, About]

firstRunNavBar :: [NavBarItem]
firstRunNavBar = [Config, About]

selectNavBar :: Handler [NavBarItem]
selectNavBar = ifM (inFirstRun) (return firstRunNavBar, return defaultNavBar)

inFirstRun :: Handler Bool
inFirstRun = isNothing . threadState <$> getYesod

{- Used instead of defaultContent; highlights the current page if it's
 - on the navbar. -}
bootstrap :: Maybe NavBarItem -> Widget -> Handler RepHtml
bootstrap navbaritem content = do
	webapp <- getYesod
	navbar <- map navdetails <$> selectNavBar
	page <- widgetToPageContent $ do
		addStylesheet $ StaticR css_bootstrap_css
		addStylesheet $ StaticR css_bootstrap_responsive_css
		addScript $ StaticR jquery_full_js
		addScript $ StaticR js_bootstrap_dropdown_js
		addScript $ StaticR js_bootstrap_modal_js
		$(widgetFile "page")
	hamletToRepHtml $(hamletFile $ hamletTemplate "bootstrap")
	where
		navdetails i = (navBarName i, navBarRoute i, Just i == navbaritem)

instance Yesod WebApp where
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

instance RenderMessage WebApp FormMessage where
	renderMessage _ _ = defaultFormMessage

type Form x = Html -> MForm WebApp WebApp (FormResult x, Widget)

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

{- Runs an Annex action from the webapp.
 -
 - When the webapp is run outside a git-annex repository, the fallback
 - value is returned.
 -}
runAnnex :: forall sub a. a -> Annex a -> GHandler sub WebApp a
runAnnex fallback a = maybe (return fallback) go =<< threadState <$> getYesod
	where
		go st = liftIO $ runThreadState st a

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

{- Adds the auth parameter as a hidden field on a form. Must be put into
 - every form. -}
webAppFormAuthToken :: Widget
webAppFormAuthToken = do
	webapp <- lift getYesod
	[whamlet|<input type="hidden" name="auth" value="#{secretToken webapp}">|]
