{- git-annex assistant webapp types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.WebApp.Types where

import Assistant.Common
import Assistant.Ssh
import Assistant.Pairing
import Assistant.Types.Buddies
import Utility.NotificationBroadcaster
import Utility.WebApp
import Utility.Yesod
import Logs.Transfer
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

mkYesodData "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

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

instance Yesod WebApp where
	{- Require an auth token be set when accessing any (non-static) route -}
	isAuthorized _ _ = checkAuthToken authToken

	{- Add the auth token to every url generated, except static subsite
	 - urls (which can show up in Permission Denied pages). -}
	joinPath = insertAuthToken authToken excludeStatic
	  where
		excludeStatic [] = True
		excludeStatic (p:_) = p /= "static"

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
		giveUrlRenderer $(hamletFile $ hamletTemplate "bootstrap")

instance RenderMessage WebApp FormMessage where
	renderMessage _ _ = defaultFormMessage

{- Runs an Annex action from the webapp.
 -
 - When the webapp is run outside a git-annex repository, the fallback
 - value is returned.
 -}
#if MIN_VERSION_yesod(1,2,0)
liftAnnexOr :: forall a. a -> Annex a -> Handler a
#else
liftAnnexOr :: forall sub a. a -> Annex a -> GHandler sub WebApp a
#endif
liftAnnexOr fallback a = ifM (noAnnex <$> getYesod)
	( return fallback
	, liftAssistant $ liftAnnex a
	)

#if MIN_VERSION_yesod(1,2,0)
instance LiftAnnex Handler where
#else
instance LiftAnnex (GHandler sub WebApp) where
#endif
	liftAnnex = liftAnnexOr $ error "internal liftAnnex"

#if MIN_VERSION_yesod(1,2,0)
instance LiftAnnex (WidgetT WebApp IO) where
#else
instance LiftAnnex (GWidget WebApp WebApp) where
#endif
	liftAnnex = liftH . liftAnnex

class LiftAssistant m where
	liftAssistant :: Assistant a -> m a

#if MIN_VERSION_yesod(1,2,0)
instance LiftAssistant Handler where
#else
instance LiftAssistant (GHandler sub WebApp) where
#endif
	liftAssistant a = liftIO . flip runAssistant a
		=<< assistantData <$> getYesod

#if MIN_VERSION_yesod(1,2,0)
instance LiftAssistant (WidgetT WebApp IO) where
#else
instance LiftAssistant (GWidget WebApp WebApp) where
#endif
	liftAssistant = liftH . liftAssistant

#if MIN_VERSION_yesod(1,2,0)
type MkMForm x = MForm Handler (FormResult x, Widget)
#else
type MkMForm x = MForm WebApp WebApp (FormResult x, Widget)
#endif

#if MIN_VERSION_yesod(1,2,0)
type MkAForm x = AForm Handler x
#else
type MkAForm x = AForm WebApp WebApp x
#endif

#if MIN_VERSION_yesod(1,2,0)
type MkField x = Monad m => RenderMessage (HandlerSite m) FormMessage => Field m x
#else
type MkField x = RenderMessage master FormMessage => Field sub master x
#endif

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

instance PathPiece Bool where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

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

instance PathPiece BuddyKey where
	toPathPiece = pack . show
	fromPathPiece = readish . unpack

instance PathPiece PairKey where
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
