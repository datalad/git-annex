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
import Git

import Yesod
import Yesod.Static
import Text.Hamlet
import Network.Socket (PortNumber)
import Text.Blaze.Renderer.Utf8
import Data.ByteString.Lazy as L

data WebApp = WebApp
	{ daemonStatus :: DaemonStatusHandle
	, baseTitle :: String
	, getStatic :: Static
	}

staticFiles "static"

mkYesod "WebApp" [parseRoutes|
/static StaticR Static getStatic
/ HomeR GET
/config ConfigR GET
|]

instance Yesod WebApp where
	defaultLayout contents = do
		page <- widgetToPageContent contents
		mmsg <- getMessage
		webapp <- getYesod
		hamletToRepHtml $(hamletFile $ hamletTemplate "default-layout")

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
	[whamlet|Hello, World<p><a href=@{ConfigR}>config|]

getConfigR :: Handler RepHtml
getConfigR = defaultLayout $ do
	setTitle "configuration"
	[whamlet|<a href=@{HomeR}>main|]

webAppThread :: ThreadState -> DaemonStatusHandle -> IO ()
webAppThread st dstatus = do
	webapp <- mkWebApp st dstatus
	app <- toWaiApp webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \port -> runThreadState st $ writeHtmlShim port

mkWebApp :: ThreadState -> DaemonStatusHandle -> IO WebApp
mkWebApp st dstatus = do
	dir <- absPath =<< runThreadState st (fromRepo repoPath)
	home <- myHomeDir
	let reldir = if dirContains home dir
		then relPathDirToFile home dir
		else dir
	let s = $(embed "static")
	return $ WebApp 
		{ daemonStatus = dstatus
		, baseTitle = reldir
		, getStatic = s
		}

{- Creates a html shim file that's used to redirect into the webapp. -}
writeHtmlShim :: PortNumber -> Annex ()
writeHtmlShim port = do
	htmlshim <- fromRepo gitAnnexHtmlShim
	liftIO $ L.writeFile htmlshim $ genHtmlShim port

{- TODO: generate this static file using Yesod. -}
genHtmlShim :: PortNumber -> L.ByteString
genHtmlShim port = renderHtml $(shamletFile $ hamletTemplate "htmlshim")
	where
		url = "http://localhost:" ++ show port ++ "/"
