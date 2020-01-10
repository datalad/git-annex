{- Git configuration
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Annex.Common
import qualified Git
import qualified Git.Config
import qualified Git.Command
import qualified Annex
import Config.Cost
import Config.DynamicConfig
import Types.Availability
import Git.Types
import qualified Types.Remote as Remote
import qualified Annex.SpecialRemote.Config as SpecialRemote
import Types.ProposedAccepted

import qualified Data.Map as M
import qualified Data.ByteString as S

type UnqualifiedConfigKey = S.ByteString

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

class RemoteNameable r where
	getRemoteName :: r -> RemoteName

instance RemoteNameable Git.Repo where
	getRemoteName r = fromMaybe "" (Git.remoteName r)

instance RemoteNameable RemoteName where
	 getRemoteName = id

instance RemoteNameable Remote where
	getRemoteName = Remote.name

instance RemoteNameable Remote.RemoteConfig where
	getRemoteName c = fromMaybe "" (SpecialRemote.lookupName c)

{- A per-remote config setting in git config. -}
remoteConfig :: RemoteNameable r => r -> UnqualifiedConfigKey -> ConfigKey
remoteConfig r key = ConfigKey $
	"remote." <> encodeBS' (getRemoteName r) <> ".annex-" <> key

{- A global annex setting in git config. -}
annexConfig :: UnqualifiedConfigKey -> ConfigKey
annexConfig key = ConfigKey ("annex." <> key)

{- Calculates cost for a remote. Either the specific default, or as configured 
 - by remote.<name>.annex-cost, or if remote.<name>.annex-cost-command
 - is set and prints a number, that is used. -}
remoteCost :: RemoteGitConfig -> Cost -> Annex Cost
remoteCost c d = fromMaybe d <$> remoteCost' c

remoteCost' :: RemoteGitConfig -> Annex (Maybe Cost)
remoteCost' = liftIO . getDynamicConfig . remoteAnnexCost

setRemoteCost :: Git.Repo -> Cost -> Annex ()
setRemoteCost r c = setConfig (remoteConfig r "cost") (show c)

setRemoteAvailability :: Git.Repo -> Availability -> Annex ()
setRemoteAvailability r c = setConfig (remoteConfig r "availability") (show c)

setRemoteIgnore :: Git.Repo -> Bool -> Annex ()
setRemoteIgnore r b = setConfig (remoteConfig r "ignore") (Git.Config.boolConfig b)

setRemoteBare :: Git.Repo -> Bool -> Annex ()
setRemoteBare r b = setConfig (remoteConfig r "bare") (Git.Config.boolConfig b)

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare

isDirect :: Annex Bool
isDirect = annexDirect <$> Annex.getGitConfig

crippledFileSystem :: Annex Bool
crippledFileSystem = annexCrippledFileSystem <$> Annex.getGitConfig

setCrippledFileSystem :: Bool -> Annex ()
setCrippledFileSystem b = do
	setConfig (annexConfig "crippledfilesystem") (Git.Config.boolConfig b)
	Annex.changeGitConfig $ \c -> c { annexCrippledFileSystem = b }

exportTree :: Remote.RemoteConfig -> Bool
exportTree c = fromMaybe False $ yesNo . fromProposedAccepted
	=<< M.lookup SpecialRemote.exportTreeField c

importTree :: Remote.RemoteConfig -> Bool
importTree c = fromMaybe False $ yesNo . fromProposedAccepted
	=<< M.lookup SpecialRemote.importTreeField c

yesNo :: String -> Maybe Bool
yesNo "yes" = Just True
yesNo "no" = Just False
yesNo _ = Nothing
