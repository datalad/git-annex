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
import Assistant.WebApp
import Assistant.WebApp.DashBoard
import Assistant.WebApp.SideBar
import Assistant.WebApp.Notifications
import Assistant.WebApp.Configurators
import Assistant.WebApp.Documentation
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
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
import Data.Text (pack, unpack)

thisThread :: String
thisThread = "WebApp"

mkYesodDispatch "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

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
			s <- newWebAppState
			return $ WebApp 
				{ threadState = st
				, daemonStatus = dstatus
				, transferQueue = transferqueue
				, secretToken = pack token
				, relDir = reldir
				, getStatic = $(embed "static")
				, webAppState = s
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
