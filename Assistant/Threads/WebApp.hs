{- git-annex assistant webapp thread
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
import Assistant.WebApp.Types
import Assistant.WebApp.DashBoard
import Assistant.WebApp.SideBar
import Assistant.WebApp.Notifications
import Assistant.WebApp.Configurators
import Assistant.WebApp.Configurators.Edit
import Assistant.WebApp.Configurators.Local
import Assistant.WebApp.Configurators.Ssh
import Assistant.WebApp.Configurators.Pairing
import Assistant.WebApp.Configurators.S3
import Assistant.WebApp.Documentation
import Assistant.WebApp.OtherRepos
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Utility.WebApp
import Utility.FileMode
import Utility.TempFile
import Git

import Yesod
import Yesod.Static
import Network.Socket (PortNumber)
import Data.Text (pack, unpack)

thisThread :: String
thisThread = "WebApp"

mkYesodDispatch "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

type Url = String

webAppThread 
	:: Maybe ThreadState
	-> DaemonStatusHandle
	-> ScanRemoteMap
	-> TransferQueue
	-> TransferSlots
	-> UrlRenderer
	-> Maybe (IO String)
	-> Maybe (Url -> FilePath -> IO ())
	-> NamedThread
webAppThread mst dstatus scanremotes transferqueue transferslots urlrenderer postfirstrun onstartup = thread $ do
	webapp <- WebApp
		<$> pure mst
		<*> pure dstatus
		<*> pure scanremotes
		<*> pure transferqueue
		<*> pure transferslots
		<*> (pack <$> genRandomToken)
		<*> getreldir mst
		<*> pure $(embed "static")
		<*> newWebAppState
		<*> pure postfirstrun
	setUrlRenderer urlrenderer $ yesodRender webapp (pack "")
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \port -> case mst of
		Nothing -> withTempFile "webapp.html" $ \tmpfile _ ->
			go port webapp tmpfile Nothing
		Just st -> do
			htmlshim <- runThreadState st $ fromRepo gitAnnexHtmlShim
			urlfile <- runThreadState st $ fromRepo gitAnnexUrlFile
			go port webapp htmlshim (Just urlfile)
	where
		thread = NamedThread thisThread
		getreldir Nothing = return Nothing
		getreldir (Just st) = Just <$>
			(relHome =<< absPath
				=<< runThreadState st (fromRepo repoPath))
		go port webapp htmlshim urlfile = do
			debug thisThread ["running on port", show port]
			let url = myUrl webapp port
			maybe noop (`writeFile` url) urlfile
			writeHtmlShim url htmlshim
			maybe noop (\a -> a url htmlshim) onstartup

{- Creates a html shim file that's used to redirect into the webapp,
 - to avoid exposing the secretToken when launching the web browser. -}
writeHtmlShim :: String -> FilePath -> IO ()
writeHtmlShim url file = viaTmp go file $ genHtmlShim url
	where
		go tmpfile content = do
			h <- openFile tmpfile WriteMode
			modifyFileMode tmpfile $ removeModes [groupReadMode, otherReadMode]
			hPutStr h content
			hClose h

{- TODO: generate this static file using Yesod. -}
genHtmlShim :: String -> String
genHtmlShim url = unlines
	[ "<html>"
	, "<head>"
	, "<title>Starting webapp...</title>"
	, "<meta http-equiv=\"refresh\" content=\"0; URL="++url++"\">"
	, "<body>"
	, "<p>"
	, "<a href=\"" ++ url ++ "\">Starting webapp...</a>"
	, "</p>"
	, "</body>"
	, "</html>"
	]

myUrl :: WebApp -> PortNumber -> Url
myUrl webapp port = unpack $ yesodRender webapp urlbase HomeR []
	where
		urlbase = pack $ "http://localhost:" ++ show port
