{- git-annex assistant unused file preferences
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Unused where

import Assistant.WebApp.Common
import qualified Annex
import qualified Git
import Config
import Config.Files
import Config.NumCopies
import Utility.DataUnits
import Git.Config
import Types.Distribution
import qualified Build.SysConfig

import qualified Data.Text as T

getConfigUnusedR :: Handler Html
getConfigUnusedR = error "TODO"

postConfigUnusedR :: Handler Html
postConfigUnusedR = getConfigUnusedR

