{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.SideBar
import Assistant.DaemonStatus
import Assistant.Threads.MountWatcher (handleMount)
import Utility.Yesod
import qualified Remote
import qualified Types.Remote as Remote
import Remote.List
import Annex.UUID (getUUID)
import Init
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Command
import qualified Annex
import Locations.UserConfig
import Utility.FreeDesktop
import Utility.Mounts
import Utility.DiskFree
import Utility.DataUnits
import Utility.Network

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import System.Posix.Directory
import qualified Control.Exception as E

getAddRemoteServerR :: Handler RepHtml
getAddRemoteServerR = bootstrap (Just Config) $ do
	error "TODO"
