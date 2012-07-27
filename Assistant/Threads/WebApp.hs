{- git-annex assistant webapp
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module Assistant.Threads.WebApp where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Utility.WebApp
import Utility.Yesod
import Utility.FileMode
import Utility.TempFile
import Git

import Yesod
import Yesod.Static
import Text.Hamlet
import Network.Socket (PortNumber)
import Text.Blaze.Renderer.String
import Data.Text
import Data.Time.Clock

thisThread :: String
thisThread = "WebApp"

data WebApp = WebApp
	{ threadState :: ThreadState
	, daemonStatus :: DaemonStatusHandle
	, secretToken :: Text
	, baseTitle :: String
	, getStatic :: Static
	}

staticFiles "static"

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
/status StatusR GET
/config ConfigR GET
/static StaticR Static getStatic
|]

instance Yesod WebApp where
	defaultLayout contents = do
		page <- widgetToPageContent contents
		mmsg <- getMessage
		webapp <- getYesod
		hamletToRepHtml $(hamletFile $ hamletTemplate "default-layout")

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

{- Add to any widget to make it auto-update.
 -
 - The widget should have a html element with id=poll, which will be
 - replaced when it's updated.
 - 
 - Updating is done by getting html from the gethtml route.
 - Or, the home route is used if the whole page has to be refreshed to
 - update.
 -
 - ms_delay is how long to delay between updates.
 - ms_startdelay is how long to delay before updating the widget at the
 - state.
 -}
autoUpdate :: Text -> Route WebApp -> Route WebApp -> Int -> Int -> Widget
autoUpdate poll gethtml home ms_delay ms_startdelay = do
	{- Fallback refreshing is provided for non-javascript browsers. -}
	let delayseconds = show $ ms_to_seconds ms_delay
	toWidgetHead $(hamletFile $ hamletTemplate "metarefresh")

	{- Use long polling to update the status display. -}
	let delay = show ms_delay
	let startdelay = show ms_startdelay
	addScript $ StaticR jquery_full_js
	$(widgetFile "longpolling")
	where
		ms_to_seconds :: Int -> Int
		ms_to_seconds ms = ceiling ((fromIntegral ms :: Double) / 1000)

{- Continually updating status display. -}
statusDisplay :: Widget
statusDisplay = do
	webapp <- lift getYesod
	time <- show <$> liftIO getCurrentTime
	
	poll <- lift newIdent
	$(widgetFile "status")
	
	autoUpdate poll StatusR HomeR (3000 :: Int) (40 :: Int)

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
	statusDisplay
	[whamlet|<p><a href="@{ConfigR}">config|]

{- Called by client to poll for a new webapp status display.
 -
 - Should block until the status has changed, and then return a div
 - containing the new status, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto the getHomeR page.
 -}
getStatusR :: Handler RepHtml
getStatusR = do
	page <- widgetToPageContent statusDisplay
	hamletToRepHtml $ [hamlet|^{pageBody page}|]

getConfigR :: Handler RepHtml
getConfigR = defaultLayout $ do
	setTitle "configuration"
	[whamlet|<a href="@{HomeR}">main|]

webAppThread :: ThreadState -> DaemonStatusHandle -> IO ()
webAppThread st dstatus = do
	webapp <- mkWebApp st dstatus
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \port -> runThreadState st $ writeHtmlShim webapp port

mkWebApp :: ThreadState -> DaemonStatusHandle -> IO WebApp
mkWebApp st dstatus = do
	dir <- absPath =<< runThreadState st (fromRepo repoPath)
	home <- myHomeDir
	let reldir = if dirContains home dir
		then relPathDirToFile home dir
		else dir
	token <- genRandomToken 
	return $ WebApp 
		{ threadState = st
		, daemonStatus = dstatus
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
