{- git-annex assistant webapp
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module Assistant.Threads.WebApp where

import Assistant.Common
import Assistant.DaemonStatus
import Utility.WebApp

import Yesod

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

webAppThread :: DaemonStatusHandle -> IO ()
webAppThread dstatus = do
	app <- toWaiApp (WebApp dstatus)
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' browser
	where
		browser p = void $
			runBrowser $ "http://" ++ localhost ++ ":" ++ show p
