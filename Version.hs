{- git-annex repository versioning
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Version where

import Control.Monad (unless)

import Types
import qualified Annex
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
	g <- Annex.gitRepo
	let v = Git.configGet g versionField ""
	if not $ null v
		then return $ Just v
		else return Nothing

setVersion :: Annex ()
setVersion = setConfig versionField defaultVersion

checkVersion :: Annex ()
checkVersion = getVersion >>= handle
	where
		handle Nothing = error "First run: git-annex init"
		handle (Just v) = unless (v `elem` supportedVersions) $
			error $ "Repository version " ++ v ++ 
				" is not supported. " ++
				msg v
		msg v
			| v `elem` upgradableVersions = "Upgrade this repository: git-annex upgrade"
			| otherwise = "Upgrade git-annex."
