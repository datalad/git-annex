{- git-annex repository versioning
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Version where

import Common.Annex
import Config

type Version = String

defaultVersion :: Version
defaultVersion = "3"

supportedVersions :: [Version]
supportedVersions = [defaultVersion]

upgradableVersions :: [Version]
upgradableVersions = ["0", "1", "2"]

versionField :: String
versionField = "annex.version"

getVersion :: Annex (Maybe Version)
getVersion = handle <$> getConfig versionField ""
	where
		handle [] = Nothing
		handle v = Just v

setVersion :: Annex ()
setVersion = setConfig versionField defaultVersion

checkVersion :: Version -> Annex ()
checkVersion v
	| v `elem` supportedVersions = noop
	| v `elem` upgradableVersions = err "Upgrade this repository: git-annex upgrade"
	| otherwise = err "Upgrade git-annex."
	where
		err msg = error $ "Repository version " ++ v ++
			" is not supported. " ++ msg
