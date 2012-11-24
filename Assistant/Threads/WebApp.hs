{- git-annex assistant webapp thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes, CPP #-}
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
import Assistant.WebApp.Configurators.AWS
#ifdef WITH_WEBDAV
import Assistant.WebApp.Configurators.WebDAV
#endif
import Assistant.WebApp.Configurators.XMPP
import Assistant.WebApp.Documentation
import Assistant.WebApp.OtherRepos
import Assistant.Types.ThreadedMonad
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
	:: AssistantData
	-> UrlRenderer
	-> Bool
	-> Maybe (IO String)
	-> Maybe (Url -> FilePath -> IO ())
	-> NamedThread
webAppThread assistantdata urlrenderer noannex postfirstrun onstartup = thread $ liftIO $ do
	webapp <- WebApp
		<$> pure assistantdata
		<*> (pack <$> genRandomToken)
		<*> getreldir
		<*> pure $(embed "static")
		<*> newWebAppState
		<*> pure postfirstrun
		<*> pure noannex
	setUrlRenderer urlrenderer $ yesodRender webapp (pack "")
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp app' $ \port -> if noannex
		then withTempFile "webapp.html" $ \tmpfile _ ->
			go port webapp tmpfile Nothing
		else do
			let st = threadState assistantdata
			htmlshim <- runThreadState st $ fromRepo gitAnnexHtmlShim
			urlfile <- runThreadState st $ fromRepo gitAnnexUrlFile
			go port webapp htmlshim (Just urlfile)
  where
	thread = NamedThread thisThread
	getreldir
		| noannex = return Nothing
		| otherwise = Just <$>
			(relHome =<< absPath
				=<< runThreadState (threadState assistantdata) (fromRepo repoPath))
	go port webapp htmlshim urlfile = do
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
