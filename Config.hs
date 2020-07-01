{- Git configuration
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Config (
	module Config,
	annexConfig,
	remoteAnnexConfig,
	remoteConfig,
) where

import Annex.Common
import qualified Git
import qualified Git.Config
import qualified Git.Command
import qualified Annex
import Config.Cost
import Config.DynamicConfig
import Types.Availability
import Types.GitConfig
import Git.Types

{- Looks up a setting in git config. This is not as efficient as using the
 - GitConfig type. -}
getConfig :: ConfigKey -> ConfigValue -> Annex ConfigValue
getConfig key d = fromRepo $ Git.Config.get key d

getConfigMaybe :: ConfigKey -> Annex (Maybe ConfigValue)
getConfigMaybe key = fromRepo $ Git.Config.getMaybe key

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig (ConfigKey key) value = do
	inRepo $ Git.Command.run
		[ Param "config"
		, Param (decodeBS' key)
		, Param value
		]
	reloadConfig

reloadConfig :: Annex ()
reloadConfig = Annex.changeGitRepo =<< inRepo Git.Config.reRead

{- Unsets a git config setting. (Leaves it in state.) -}
unsetConfig :: ConfigKey -> Annex ()
unsetConfig key = void $ inRepo $ Git.Config.unset key

{- Calculates cost for a remote. Either the specific default, or as configured 
 - by remote.<name>.annex-cost, or if remote.<name>.annex-cost-command
 - is set and prints a number, that is used. -}
remoteCost :: RemoteGitConfig -> Cost -> Annex Cost
remoteCost c d = fromMaybe d <$> remoteCost' c

remoteCost' :: RemoteGitConfig -> Annex (Maybe Cost)
remoteCost' = liftIO . getDynamicConfig . remoteAnnexCost

setRemoteCost :: Git.Repo -> Cost -> Annex ()
setRemoteCost r c = setConfig (remoteAnnexConfig r "cost") (show c)

setRemoteAvailability :: Git.Repo -> Availability -> Annex ()
setRemoteAvailability r c = setConfig (remoteAnnexConfig r "availability") (show c)

setRemoteIgnore :: Git.Repo -> Bool -> Annex ()
setRemoteIgnore r b = setConfig (remoteAnnexConfig r "ignore") (Git.Config.boolConfig b)

setRemoteBare :: Git.Repo -> Bool -> Annex ()
setRemoteBare r b = setConfig (remoteAnnexConfig r "bare") (Git.Config.boolConfig b)

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare

isDirect :: Annex Bool
isDirect = annexDirect <$> Annex.getGitConfig

crippledFileSystem :: Annex Bool
crippledFileSystem = annexCrippledFileSystem <$> Annex.getGitConfig

setCrippledFileSystem :: Bool -> Annex ()
setCrippledFileSystem b =
	setConfig (annexConfig "crippledfilesystem") (Git.Config.boolConfig b)

pidLockFile :: Annex (Maybe FilePath)
#ifndef mingw32_HOST_OS
pidLockFile = ifM (annexPidLock <$> Annex.getGitConfig)
	( Just <$> Annex.fromRepo gitAnnexPidLockFile
	, pure Nothing
	)
#else
pidLockFile = pure Nothing
#endif
