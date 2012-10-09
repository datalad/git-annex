{- git-annex assistant webapp configurator for editing existing repos
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Edit where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Assistant.Sync
import Assistant.MakeRemote
import Utility.Yesod
import Init
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Annex
import Locations.UserConfig
import Utility.FreeDesktop
import Utility.Mounts
import Utility.DiskFree
import Utility.DataUnits
import Utility.Network
import Remote (prettyListUUIDs)
import Logs.Group
import Annex.UUID

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Char
import System.Posix.Directory
import qualified Control.Exception as E

getEditRepositoryR :: UUID -> Handler RepHtml
getEditRepositoryR u = error "TODO"
