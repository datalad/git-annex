{- git-annex assistant webapp configurators for WebDAV remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.WebDAV where

import Assistant.Common
import Assistant.MakeRemote
import Assistant.Sync
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod
import qualified Remote.WebDAV as WebDAV
import qualified Remote
import Types.Remote (RemoteConfig)
import Types.StandardGroups
import Logs.PreferredContent
import Logs.Remote

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

webDAVConfigurator :: Widget -> Handler RepHtml
webDAVConfigurator a = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a WebDAV repository"
	a

boxConfigurator :: Widget -> Handler RepHtml
boxConfigurator a = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a Box.com repository"
	a

data WebDAVInput = WebDAVInput
	{ user :: Text
	, password :: Text
	, directory :: Text
	}

boxComAForm :: AForm WebApp WebApp WebDAVInput
boxComAForm = WebDAVInput
	<$> areq textField "Username or Email" Nothing
	<*> areq passwordField "Box.com Password" Nothing
	<*> areq textField "Directory" (Just "annex")

webDAVCredsAForm :: AForm WebApp WebApp WebDAVInput
webDAVCredsAForm = WebDAVInput
	<$> areq textField "Username or Email" Nothing
	<*> areq passwordField "Password" Nothing
	<*> pure (T.empty)

getAddBoxComR :: Handler RepHtml
getAddBoxComR = boxConfigurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap boxComAForm
	case result of
		FormSuccess input -> lift $ 
			makeWebDavRemote "box.com" input setgroup $ M.fromList
				[ ("encryption", "shared")
				, ("type", "webdav")
				, ("url", "https://www.box.com/dav/" ++ T.unpack (directory input))
				-- Box.com has a max file size of 100 mb, but
				-- using smaller chunks has better memory
				-- performance.
				, ("chunksize", "10mb")
				]
		_ -> do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/addbox.com")
  where
	setgroup r = runAnnex () $
		setStandardGroup (Remote.uuid r) TransferGroup

getEnableWebDAVR :: UUID -> Handler RepHtml
getEnableWebDAVR uuid = do
	m <- runAnnex M.empty readRemoteLog
	let c = fromJust $ M.lookup uuid m
	let name = fromJust $ M.lookup "name" c
	let url = fromJust $ M.lookup "url" c
	go name url
  where
	go name url
		| "box.com/" `isInfixOf` url = boxConfigurator $ enable name url
		| otherwise = webDAVConfigurator $ enable name url

	enable name url = do
		((result, form), enctype) <- lift $
			runFormGet $ renderBootstrap webDAVCredsAForm
		case result of
			FormSuccess creds -> lift $
				makeWebDavRemote name creds (const noop) M.empty
			_ -> do
				let authtoken = webAppFormAuthToken
				description <- lift $ runAnnex "" $
					T.pack . concat <$> Remote.prettyListUUIDs [uuid]
				$(widgetFile "configurators/enablewebdav")

makeWebDavRemote :: String -> WebDAVInput -> (Remote -> Handler ()) -> RemoteConfig -> Handler ()
makeWebDavRemote name input setup config = do
	remotename <- runAnnex name $ fromRepo $ uniqueRemoteName name 0
	liftIO $ WebDAV.setCredsEnv (T.unpack $ user input, T.unpack $ password input)
	r <- liftAssistant $ liftAnnex $ addRemote $ do
		makeSpecialRemote name WebDAV.remote config
		return remotename
	setup r
	liftAssistant $ syncNewRemote r
	redirect $ EditNewCloudRepositoryR $ Remote.uuid r
