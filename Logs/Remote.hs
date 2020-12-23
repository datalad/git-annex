{- git-annex remote log
 - 
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Remote (
	remoteLog,
	remoteConfigMap,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,
	showConfig,

	prop_isomorphic_configEscape,
	prop_parse_show_Config,
) where

import Annex.Common
import qualified Annex
import qualified Annex.Branch
import Types.Remote
import Logs
import Logs.UUIDBased
import Logs.Remote.Pure
import Annex.SpecialRemote.Config

import qualified Data.Map as M

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u cfg = do
	c <- currentVectorClock
	Annex.Branch.change remoteLog $
		buildRemoteConfigLog
			. changeLog c u (removeSameasInherited cfg)
			. parseRemoteConfigLog
	Annex.changeState $ \s -> s { Annex.remoteconfigmap = Nothing }

{- Map of remotes by uuid containing key/value config maps.
 - Cached for speed. -}
remoteConfigMap :: Annex (M.Map UUID RemoteConfig)
remoteConfigMap = maybe remoteConfigMapLoad return
	=<< Annex.getState Annex.remoteconfigmap

remoteConfigMapLoad :: Annex (M.Map UUID RemoteConfig)
remoteConfigMapLoad = do
	m <- readRemoteLog
	Annex.changeState $ \s -> s { Annex.remoteconfigmap = Just m }
	return m

readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = calcRemoteConfigMap 
	<$> Annex.Branch.get remoteLog
