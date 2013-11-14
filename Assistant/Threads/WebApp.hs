{- git-annex assistant webapp thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.Threads.WebApp where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.DashBoard
import Assistant.WebApp.SideBar
import Assistant.WebApp.Notifications
import Assistant.WebApp.RepoList
import Assistant.WebApp.Configurators
import Assistant.WebApp.Configurators.Local
import Assistant.WebApp.Configurators.Ssh
import Assistant.WebApp.Configurators.Pairing
import Assistant.WebApp.Configurators.AWS
import Assistant.WebApp.Configurators.IA
import Assistant.WebApp.Configurators.WebDAV
import Assistant.WebApp.Configurators.XMPP
import Assistant.WebApp.Configurators.Preferences
import Assistant.WebApp.Configurators.Edit
import Assistant.WebApp.Configurators.Delete
import Assistant.WebApp.Configurators.Fsck
import Assistant.WebApp.Documentation
import Assistant.WebApp.Control
import Assistant.WebApp.OtherRepos
import Assistant.WebApp.Repair
import Assistant.Types.ThreadedMonad
import Utility.WebApp
import Utility.Tmp
import Utility.FileMode
import Git

import Yesod
import Network.Socket (SockAddr, HostName)
import Data.Text (pack, unpack)

mkYesodDispatch "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

type Url = String

webAppThread
	:: AssistantData
	-> UrlRenderer
	-> Bool
	-> Maybe HostName
	-> Maybe (IO Url)
	-> Maybe (Url -> FilePath -> IO ())
	-> NamedThread
webAppThread assistantdata urlrenderer noannex listenhost postfirstrun onstartup = thread $ liftIO $ do
#ifdef __ANDROID__
	when (isJust listenhost) $
		-- See Utility.WebApp
		error "Sorry, --listen is not currently supported on Android"
#endif
	webapp <- WebApp
		<$> pure assistantdata
		<*> (pack <$> genRandomToken)
		<*> getreldir
		<*> pure staticRoutes
		<*> pure postfirstrun
		<*> pure noannex
		<*> pure listenhost
	setUrlRenderer urlrenderer $ yesodRender webapp (pack "")
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ httpDebugLogger app
		, return app
		)
	runWebApp listenhost app' $ \addr -> if noannex
		then withTmpFile "webapp.html" $ \tmpfile _ ->
			go addr webapp tmpfile Nothing
		else do
			let st = threadState assistantdata
			htmlshim <- runThreadState st $ fromRepo gitAnnexHtmlShim
			urlfile <- runThreadState st $ fromRepo gitAnnexUrlFile
			go addr webapp htmlshim (Just urlfile)
  where
  	-- The webapp thread does not wait for the startupSanityCheckThread
	-- to finish, so that the user interface remains responsive while
	-- that's going on.
	thread = namedThreadUnchecked "WebApp"
	getreldir
		| noannex = return Nothing
		| otherwise = Just <$>
			(relHome =<< absPath
				=<< runThreadState (threadState assistantdata) (fromRepo repoPath))
	go addr webapp htmlshim urlfile = do
		let url = myUrl webapp addr
		maybe noop (`writeFileProtected` url) urlfile
		writeHtmlShim "Starting webapp..." url htmlshim
		maybe noop (\a -> a url htmlshim) onstartup

myUrl :: WebApp -> SockAddr -> Url
myUrl webapp addr = unpack $ yesodRender webapp urlbase DashboardR []
  where
	urlbase = pack $ "http://" ++ show addr
