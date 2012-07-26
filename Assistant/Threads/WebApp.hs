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

thisThread :: String
thisThread = "WebApp"

data WebApp = WebApp
	{ daemonStatus :: DaemonStatusHandle
	, secretToken :: Text
	, baseTitle :: String
	, getStatic :: Static
	}

staticFiles "static"

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
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

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
	[whamlet|Hello, World<p><a href="@{ConfigR}">config|]

getConfigR :: Handler RepHtml
getConfigR = defaultLayout $ do
	setTitle "configuration"
	[whamlet|<a href="@{HomeR}">main|]

webAppThread :: ThreadState -> DaemonStatusHandle -> IO ()
webAppThread st dstatus = do
	webapp <- mkWebApp st dstatus
	app <- toWaiApp webapp
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
		{ daemonStatus = dstatus
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
