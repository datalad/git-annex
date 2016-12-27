{- git-annex assistant webapp thread
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
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
import Assistant.WebApp.Configurators.Preferences
import Assistant.WebApp.Configurators.Unused
import Assistant.WebApp.Configurators.Edit
import Assistant.WebApp.Configurators.Delete
import Assistant.WebApp.Configurators.Fsck
import Assistant.WebApp.Configurators.Upgrade
import Assistant.WebApp.Documentation
import Assistant.WebApp.Control
import Assistant.WebApp.OtherRepos
import Assistant.WebApp.Repair
import Assistant.WebApp.Pairing
import Assistant.Types.ThreadedMonad
import Utility.WebApp
import Utility.AuthToken
import Utility.Tmp
import Utility.FileMode
import Git
import qualified Annex

import Yesod
import Network.Socket (SockAddr, HostName)
import Data.Text (pack, unpack)
import qualified Network.Wai.Handler.WarpTLS as TLS
import Network.Wai.Middleware.RequestLogger

mkYesodDispatch "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

type Url = String

webAppThread
	:: AssistantData
	-> UrlRenderer
	-> Bool
	-> Maybe String
	-> Maybe (IO Url)
	-> Maybe HostName
	-> Maybe (Url -> FilePath -> IO ())
	-> NamedThread
webAppThread assistantdata urlrenderer noannex cannotrun postfirstrun listenhost onstartup = thread $ liftIO $ do
	listenhost' <- if isJust listenhost
		then pure listenhost
		else getAnnex $ annexListen <$> Annex.getGitConfig
	tlssettings <- getAnnex getTlsSettings
#ifdef __ANDROID__
	when (isJust listenhost') $
		-- See Utility.WebApp
		giveup "Sorry, --listen is not currently supported on Android"
#endif
	webapp <- WebApp
		<$> pure assistantdata
		<*> genAuthToken 128
		<*> getreldir
		<*> pure staticRoutes
		<*> pure postfirstrun
		<*> pure cannotrun
		<*> pure noannex
		<*> pure listenhost'
		<*> newWormholePairingState
	setUrlRenderer urlrenderer $ yesodRender webapp (pack "")
	app <- toWaiAppPlain webapp
	app' <- ifM debugEnabled
		( return $ logStdout app
		, return app
		)
	runWebApp tlssettings listenhost' app' $ \addr -> if noannex
		then withTmpFile "webapp.html" $ \tmpfile h -> do
			hClose h
			go tlssettings addr webapp tmpfile Nothing
		else do
			htmlshim <- getAnnex' $ fromRepo gitAnnexHtmlShim
			urlfile <- getAnnex' $ fromRepo gitAnnexUrlFile
			go tlssettings addr webapp htmlshim (Just urlfile)
  where
	-- The webapp thread does not wait for the startupSanityCheckThread
	-- to finish, so that the user interface remains responsive while
	-- that's going on.
	thread = namedThreadUnchecked "WebApp"
	getreldir
		| noannex = return Nothing
		| otherwise = Just <$>
			(relHome =<< absPath
				=<< getAnnex' (fromRepo repoPath))
	go tlssettings addr webapp htmlshim urlfile = do
		let url = myUrl tlssettings webapp addr
		maybe noop (`writeFileProtected` url) urlfile
		writeHtmlShim "Starting webapp..." url htmlshim
		maybe noop (\a -> a url htmlshim) onstartup

	getAnnex a
		| noannex = pure Nothing
		| otherwise = getAnnex' a
	getAnnex' = runThreadState (threadState assistantdata)

myUrl :: Maybe TLS.TLSSettings -> WebApp -> SockAddr -> Url
myUrl tlssettings webapp addr = unpack $ yesodRender webapp urlbase DashboardR []
  where
	urlbase = pack $ proto ++ "://" ++ show addr
	proto
		| isJust tlssettings = "https"
		| otherwise = "http"

getTlsSettings :: Annex (Maybe TLS.TLSSettings)
getTlsSettings = do
	cert <- fromRepo gitAnnexWebCertificate
	privkey <- fromRepo gitAnnexWebPrivKey
	ifM (liftIO $ allM doesFileExist [cert, privkey])
		( return $ Just $ TLS.tlsSettings cert privkey
		, return Nothing
		)
