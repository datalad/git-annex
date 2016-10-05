{- git-annex repository versioning
 -
 - Copyright 2010,2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Version where

import Annex.Common
import Config
import qualified Annex

type Version = String

defaultVersion :: Version
defaultVersion = "5"

latestVersion :: Version
latestVersion = "6"

supportedVersions :: [Version]
supportedVersions = ["5", "6"]

versionForAdjustedClone :: Version
versionForAdjustedClone = "6"

upgradableVersions :: [Version]
#ifndef mingw32_HOST_OS
upgradableVersions = ["0", "1", "2", "3", "4", "5"]
#else
upgradableVersions = ["2", "3", "4", "5"]
#endif

autoUpgradeableVersions :: [Version]
autoUpgradeableVersions = ["3", "4"]

versionField :: ConfigKey
versionField = annexConfig "version"

getVersion :: Annex (Maybe Version)
getVersion = annexVersion <$> Annex.getGitConfig

versionSupportsDirectMode :: Annex Bool
versionSupportsDirectMode = go <$> getVersion
  where
	go (Just "6") = False
	go _ = True

versionSupportsUnlockedPointers :: Annex Bool
versionSupportsUnlockedPointers = go <$> getVersion
  where
	go (Just "6") = True
	go _ = False

versionSupportsAdjustedBranch :: Annex Bool
versionSupportsAdjustedBranch = versionSupportsUnlockedPointers

versionUsesKeysDatabase :: Annex Bool
versionUsesKeysDatabase = versionSupportsUnlockedPointers

setVersion :: Version -> Annex ()
setVersion = setConfig versionField

removeVersion :: Annex ()
removeVersion = unsetConfig versionField
