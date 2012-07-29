{- git-annex assistant webapp
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.Threads.WebApp where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Utility.NotificationBroadcaster
import Utility.WebApp
import Utility.Yesod
import Utility.FileMode
import Utility.TempFile
import Git
import Logs.Transfer
import Utility.Percentage
import Utility.DataUnits
import Types.Key
import qualified Remote

import Yesod
import Yesod.Static
import Text.Hamlet
import Network.Socket (PortNumber)
import Text.Blaze.Renderer.String
import Data.Text (Text, pack, unpack)
import qualified Data.Map as M
import Data.Time.Clock

thisThread :: String
thisThread = "WebApp"

data WebApp = WebApp
	{ threadState :: ThreadState
	, daemonStatus :: DaemonStatusHandle
	, transferQueue :: TransferQueue
	, secretToken :: Text
	, baseTitle :: String
	, getStatic :: Static
	}

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

staticFiles "static"

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
/noscript NoScriptR GET
/noscriptauto NoScriptAutoR GET
/transfers/#NotificationId TransfersR GET
/sidebar/#NotificationId SideBarR GET
/config ConfigR GET
/static StaticR Static getStatic
|]

instance PathPiece NotificationId where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance Yesod WebApp where
	defaultLayout content = do
		webapp <- getYesod
		page <- widgetToPageContent $ do
			addStylesheet $ StaticR css_bootstrap_css
			addStylesheet $ StaticR css_bootstrap_responsive_css
			addScript $ StaticR jquery_full_js
			addScript $ StaticR js_bootstrap_dropdown_js
			addScript $ StaticR js_bootstrap_alert_js
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

{- Add to any widget to make it auto-update using long polling.
 -
 - The widget should have a html element with an id=ident, which will be
 - replaced when it's updated.
 - 
 - Updating is done by getting html from the gethtml route.
 -
 - ms_delay is how long to delay between AJAX updates
 - ms_startdelay is how long to delay before updating with AJAX at the start
 -}
autoUpdate :: Text -> Route WebApp -> Int -> Int -> Widget
autoUpdate ident gethtml ms_delay ms_startdelay = do
	let delay = show ms_delay
	let startdelay = show ms_startdelay
	$(widgetFile "longpolling")

{- A display of currently running and queued transfers. -}
transfersDisplay :: Bool -> Widget
transfersDisplay warnNoScript = do
	webapp <- lift getYesod
	current <- liftIO $ runThreadState (threadState webapp) $
		M.toList . currentTransfers
			<$> liftIO (getDaemonStatus $ daemonStatus webapp)
	queued <- liftIO $ getTransferQueue $ transferQueue webapp
	let transfers = current ++ queued
	let ident = transfersDisplayIdent
	$(widgetFile "transfers")

transfersDisplayIdent :: Text
transfersDisplayIdent = "transfers"

{- Called by client to get a display of currently in process transfers.
 -
 - Returns a div, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto the getHomeR page.
 -}
getTransfersR :: NotificationId -> Handler RepHtml
getTransfersR nid = do
	waitNotifier transferNotifier nid

	page <- widgetToPageContent $ transfersDisplay False
	hamletToRepHtml $ [hamlet|^{pageBody page}|]

sideBarDisplay :: Bool -> Widget
sideBarDisplay noScript = do
	date <- liftIO $ show <$> getCurrentTime
	ident <- lift newIdent
	mmsg <- lift getMessage
	$(widgetFile "sidebar")
	unless noScript $ do
		{- Set up automatic updates of the sidebar. -}
		nid <- lift $ newNotifier transferNotifier
		autoUpdate ident (SideBarR nid) (10 :: Int) (10 :: Int)

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
	waitNotifier transferNotifier nid

	page <- widgetToPageContent $ sideBarDisplay True
	hamletToRepHtml $ [hamlet|^{pageBody page}|]

dashboard :: Bool -> Bool -> Widget
dashboard noScript warnNoScript = do
	sideBarDisplay noScript
	transfersDisplay warnNoScript

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
	{- Set up automatic updates for the transfers display. -}
	nid <- lift $ newNotifier transferNotifier
	autoUpdate transfersDisplayIdent (TransfersR nid) (10 :: Int) (10 :: Int)
	
	dashboard False True

{- Same as HomeR, except with no javascript, so it doesn't allocate
 - new resources each time the page is refreshed, and with autorefreshing
 - via meta refresh. -}
getNoScriptAutoR :: Handler RepHtml
getNoScriptAutoR = defaultLayout $ do
	let ident = NoScriptR
	let delayseconds = 3 :: Int
	let this = NoScriptAutoR
	toWidgetHead $(hamletFile $ hamletTemplate "metarefresh")
	dashboard True False

getNoScriptR :: Handler RepHtml
getNoScriptR = defaultLayout $
	dashboard True True

getConfigR :: Handler RepHtml
getConfigR = defaultLayout $ do
	sideBarDisplay False
	setTitle "configuration"
	[whamlet|<a href="@{HomeR}">main|]

webAppThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Maybe (IO ()) -> IO ()
webAppThread st dstatus transferqueue onstartup = do
	webapp <- mkWebApp
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \port -> do
		runThreadState st $ writeHtmlShim webapp port
		maybe noop id onstartup
	where
		mkWebApp = do
			dir <- absPath =<< runThreadState st (fromRepo repoPath)
			home <- myHomeDir
			let reldir = if dirContains home dir
				then relPathDirToFile home dir
				else dir
			token <- genRandomToken 
			return $ WebApp 
				{ threadState = st
				, daemonStatus = dstatus
				, transferQueue = transferqueue
				, secretToken = pack token
				, baseTitle = reldir
				, getStatic = $(embed "static")
				}

{- Creates a html shim file that's used to redirect into the webapp,
 - to avoid exposing the secretToken when launching the web browser. -}
writeHtmlShim :: WebApp -> PortNumber -> Annex ()
writeHtmlShim webapp port = do
	liftIO $ debug thisThread ["running on port", show port]
	htmlshim <- fromRepo gitAnnexHtmlShim
	liftIO $ viaTmp go htmlshim $ genHtmlShim webapp port
	where
		go file content = do
			h <- openFile file WriteMode
			modifyFileMode file $ removeModes [groupReadMode, otherReadMode]
			hPutStr h content
			hClose h

{- TODO: generate this static file using Yesod. -}
genHtmlShim :: WebApp -> PortNumber -> String
genHtmlShim webapp port = renderHtml $(shamletFile $ hamletTemplate "htmlshim")
	where
		url = "http://localhost:" ++ show port ++
			"/?auth=" ++ unpack (secretToken webapp)
