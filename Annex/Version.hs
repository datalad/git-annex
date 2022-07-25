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
supportedVersions = map RepoVersion [8, 9, 10]

upgradeableVersions :: [RepoVersion]
#ifndef mingw32_HOST_OS
upgradeableVersions = map RepoVersion [0..10]
#else
upgradeableVersions = map RepoVersion [2..10]
#endif

autoUpgradeableVersions :: M.Map RepoVersion RepoVersion
autoUpgradeableVersions = M.fromList
	[ (RepoVersion 3, defaultVersion)
	, (RepoVersion 4, defaultVersion)
	, (RepoVersion 5, defaultVersion)
	, (RepoVersion 6, defaultVersion)
	, (RepoVersion 7, defaultVersion)
	, (RepoVersion 8, defaultVersion)
	, (RepoVersion 9, defaultVersion)
	]

versionField :: ConfigKey
versionField = annexConfig "version"

getVersion :: Annex (Maybe RepoVersion)
getVersion = annexVersion <$> Annex.getGitConfig

setVersion :: RepoVersion -> Annex ()
setVersion (RepoVersion v) = setConfig versionField (show v)

removeVersion :: Annex ()
removeVersion = unsetConfig versionField

versionSupportsFilterProcess :: Maybe RepoVersion -> Bool
versionSupportsFilterProcess (Just v) 
	| v >= RepoVersion 9 = True
versionSupportsFilterProcess _ = False

versionNeedsWritableContentFiles :: Maybe RepoVersion -> Bool
versionNeedsWritableContentFiles (Just v) 
	| v >= RepoVersion 10 = False
versionNeedsWritableContentFiles _ = True
