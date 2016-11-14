{- git-annex assistant webapp types
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.WebApp.Types where

import Assistant.Common
import Assistant.Ssh
import Assistant.Pairing
import Utility.NotificationBroadcaster
import Utility.WebApp
import Utility.Yesod
import Types.Transfer
import Utility.Gpg (KeyId)
import Build.SysConfig (packageversion)
import Types.ScheduledActivity
import Assistant.WebApp.RepoId
import Types.Distribution

import Yesod.Static
import Text.Hamlet
import Data.Text (Text, pack, unpack)
import Network.Socket (HostName)

publicFiles "static"

staticRoutes :: Static
staticRoutes = $(embed "static")

data WebApp = WebApp
	{ assistantData :: AssistantData
	, authToken :: AuthToken
	, relDir :: Maybe FilePath
	, getStatic :: Static
	, postFirstRun :: Maybe (IO String)
	, cannotRun :: Maybe String
	, noAnnex :: Bool
	, listenHost ::Maybe HostName
	}

mkYesodData "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

excludeStatic :: [Text] -> Bool
excludeStatic [] = True
excludeStatic (p:_) = p /= "static"

instance Yesod WebApp where
	{- Require an auth token be set when accessing any (non-static) route -}
	isAuthorized r _ = checkAuthToken authToken r excludeStatic

	{- Add the auth token to every url generated, except static subsite
	 - urls (which can show up in Permission Denied pages). -}
	joinPath = insertAuthToken authToken excludeStatic

	makeSessionBackend = webAppSessionBackend
	jsLoader _ = BottomOfHeadBlocking

	{- The webapp does not use defaultLayout, so this is only used
	 - for error pages or any other built-in yesod page.
	 - 
	 - This can use static routes, but should use no other routes,
	 - as that would expose the auth token.
	 -}
	defaultLayout content = do
		webapp <- getYesod
		pageinfo <- widgetToPageContent $ do
			addStylesheet $ StaticR css_bootstrap_css
			addStylesheet $ StaticR css_bootstrap_theme_css
			addScript $ StaticR js_jquery_full_js
			addScript $ StaticR js_bootstrap_js
			$(widgetFile "error")
		withUrlRenderer $(hamletFile $ hamletTemplate "bootstrap")

instance RenderMessage WebApp FormMessage where
	renderMessage _ _ = defaultFormMessage

instance LiftAnnex Handler where
	liftAnnex a = ifM (noAnnex <$> getYesod)
		( error "internal liftAnnex"
		, liftAssistant $ liftAnnex a
		)

instance LiftAnnex (WidgetT WebApp IO) where
	liftAnnex = liftH . liftAnnex

class LiftAssistant m where
	liftAssistant :: Assistant a -> m a

instance LiftAssistant Handler where
	liftAssistant a = liftIO . flip runAssistant a
		=<< assistantData <$> getYesod

instance LiftAssistant (WidgetT WebApp IO) where
	liftAssistant = liftH . liftAssistant

type MkMForm x = MForm Handler (FormResult x, Widget)

type MkAForm x = AForm Handler x

type MkField x = forall m. Monad m => RenderMessage (HandlerSite m) FormMessage => Field m x

data RepoSelector = RepoSelector
	{ onlyCloud :: Bool
	, onlyConfigured :: Bool
	, includeHere :: Bool
	, nudgeAddMore :: Bool
	}
	deriving (Read, Show, Eq)

data RemovableDrive = RemovableDrive 
	{ diskFree :: Maybe Integer
	, mountPoint :: Text
	, driveRepoPath :: Text
	}
	deriving (Read, Show, Eq, Ord)

data RepoKey = RepoKey KeyId | NoRepoKey
	deriving (Read, Show, Eq, Ord)

instance PathPiece RemovableDrive where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece RepoKey where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece SshData where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece NotificationId where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece AlertId where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece Transfer where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece PairMsg where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece SecretReminder where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece UUID where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece RepoSelector where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece ThreadName where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece ScheduledActivity where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece RepoId where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece GitAnnexDistribution where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack
