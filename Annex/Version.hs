{- git-annex repository versioning
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Annex.Version where

import Annex.Common
import Config
import Git.Types
import Types.RepoVersion
import qualified Annex

import qualified Data.Map as M

defaultVersion :: RepoVersion
defaultVersion = RepoVersion 10

latestVersion :: RepoVersion
latestVersion = RepoVersion 10

supportedVersions :: [RepoVersion]
supportedVersions = map RepoVersion [9, 10]

upgradeableVersions :: [RepoVersion]
#ifndef mingw32_HOST_OS
upgradeableVersions = map RepoVersion [0..10]
#else
upgradeableVersions = map RepoVersion [2..10]
#endif

autoUpgradeableVersions :: M.Map RepoVersion RepoVersion
autoUpgradeableVersions = M.fromList
	[ (RepoVersion 3, latestVersion)
	, (RepoVersion 4, latestVersion)
	, (RepoVersion 5, latestVersion)
	, (RepoVersion 6, latestVersion)
	, (RepoVersion 7, latestVersion)
	, (RepoVersion 8, latestVersion) 
	, (RepoVersion 9, latestVersion) 
	]

versionField :: ConfigKey
versionField = annexConfig "version"

getVersion :: Annex (Maybe RepoVersion)
getVersion = annexVersion <$> Annex.getGitConfig

setVersion :: RepoVersion -> Annex ()
setVersion (RepoVersion v) = setConfig versionField (show v)

removeVersion :: Annex ()
removeVersion = unsetConfig versionField

versionNeedsWritableContentFiles :: Maybe RepoVersion -> Bool
versionNeedsWritableContentFiles (Just v) 
	| v >= RepoVersion 10 = False
versionNeedsWritableContentFiles _ = True
