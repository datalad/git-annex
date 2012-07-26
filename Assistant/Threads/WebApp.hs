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

import Yesod
import Network.Socket (PortNumber)

data WebApp = WebApp DaemonStatusHandle

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
/config ConfigR GET
|]

instance Yesod WebApp

getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|Hello, World<p><a href=@{ConfigR}>config|]

getConfigR :: Handler RepHtml
getConfigR = defaultLayout [whamlet|<a href=@{HomeR}>main|]

webAppThread :: ThreadState -> DaemonStatusHandle -> IO ()
webAppThread st dstatus = do
	app <- toWaiApp (WebApp dstatus)
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \p -> runThreadState st $ writeHtmlShim p

{- Creates a html shim file that's used to redirect into the webapp. -}
writeHtmlShim :: PortNumber -> Annex ()
writeHtmlShim port = do
	htmlshim <- fromRepo gitAnnexHtmlShim
	liftIO $ writeFile htmlshim $ genHtmlShim port

{- TODO: generate this static file using Yesod. -}
genHtmlShim :: PortNumber -> String
genHtmlShim port = unlines
	[ "<html>"
	, "<head>"
	, "<meta http-equiv=\"refresh\" content=\"0; URL=" ++ url ++ "\">"
	, "</head>"
	, "<body>"
	, "<p>"
	, "<a href=\"" ++ url ++ "\">Starting webapp...</a>"
	, "</p>"
	, "</body>"
	]
	where
		url = "http://localhost:" ++ show port ++ "/"
