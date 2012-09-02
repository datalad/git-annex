{- git-annex assistant webapp types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.WebApp.Types where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Alert
import Utility.NotificationBroadcaster
import Utility.WebApp
import Logs.Transfer

import Yesod
import Yesod.Static
import Data.Text (Text, pack, unpack)
import Control.Concurrent.STM

staticFiles "static"

mkYesodData "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

data WebApp = WebApp
	{ threadState :: Maybe ThreadState
	, daemonStatus :: DaemonStatusHandle
	, scanRemotes :: ScanRemoteMap
	, transferQueue :: TransferQueue
	, transferSlots :: TransferSlots
	, secretToken :: Text
	, relDir :: Maybe FilePath
	, getStatic :: Static
	, webAppState :: TMVar WebAppState
	, postFirstRun :: Maybe (IO String)
	}

instance Yesod WebApp where
	{- Require an auth token be set when accessing any (non-static route) -}
	isAuthorized _ _ = checkAuthToken secretToken

	{- Add the auth token to every url generated, except static subsite
         - urls (which can show up in Permission Denied pages). -}
	joinPath = insertAuthToken secretToken excludeStatic
		where
			excludeStatic [] = True
			excludeStatic (p:_) = p /= "static"

	makeSessionBackend = webAppSessionBackend
	jsLoader _ = BottomOfHeadBlocking

instance RenderMessage WebApp FormMessage where
	renderMessage _ _ = defaultFormMessage

type Form x = Html -> MForm WebApp WebApp (FormResult x, Widget)

data WebAppState = WebAppState
	{ showIntro :: Bool
	}

type PubKey = String

data SshData = SshData
	{ sshHostName :: Text
	, sshUserName :: Maybe Text
	, sshDirectory :: Text
	, sshRepoName :: String
	, pubKey :: Maybe PubKey
	, rsyncOnly :: Bool
	}
	deriving (Read, Show, Eq)

{- Allow any serializable data type to be used as a PathPiece -}
instance (Show a, Read a) => PathPiece a where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack
