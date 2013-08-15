{- git-annex repository versioning
 -
 - Copyright 2010,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Version where

import Common.Annex
import Config
import qualified Annex

type Version = String

defaultVersion :: Version
defaultVersion = "3"

directModeVersion :: Version
directModeVersion = "4"

supportedVersions :: [Version]
supportedVersions = [defaultVersion, directModeVersion]

upgradableVersions :: [Version]
#ifndef mingw32_HOST_OS
upgradableVersions = ["0", "1", "2"]
#else
upgradableVersions = ["2"]
#endif

versionField :: ConfigKey
versionField = annexConfig "version"

getVersion :: Annex (Maybe Version)
getVersion = annexVersion <$> Annex.getGitConfig

setVersion :: Version -> Annex ()
setVersion = setConfig versionField

removeVersion :: Annex ()
removeVersion = unsetConfig versionField

checkVersion :: Version -> Annex ()
checkVersion v
	| v `elem` supportedVersions = noop
	| v `elem` upgradableVersions = err "Upgrade this repository: git-annex upgrade"
	| otherwise = err "Upgrade git-annex."
  where
	err msg = error $ "Repository version " ++ v ++
		" is not supported. " ++ msg
