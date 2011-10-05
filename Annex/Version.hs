{- git-annex repository versioning
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Version where

import Common.Annex
import qualified Git
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
getVersion = do
	g <- gitRepo
	let v = Git.configGet g versionField ""
	if not $ null v
		then return $ Just v
		else return Nothing

setVersion :: Annex ()
setVersion = setConfig versionField defaultVersion

checkVersion :: Version -> Annex ()
checkVersion v
	| v `elem` supportedVersions = return ()
	| v `elem` upgradableVersions = err "Upgrade this repository: git-annex upgrade"
	| otherwise = err "Upgrade git-annex."
	where
		err msg = error $ "Repository version " ++ v ++
			" is not supported. " ++ msg
