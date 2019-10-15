{- git-annex remote log
 - 
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Remote (
	remoteLog,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,
	showConfig,

	prop_isomorphic_configEscape,
	prop_parse_show_Config,
) where

import Annex.Common
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
	c <- liftIO currentVectorClock
	Annex.Branch.change remoteLog $
		buildRemoteConfigLog
			. changeLog c u (removeSameasInherited cfg)
			. parseRemoteConfigLog

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = calcRemoteConfigMap 
	<$> Annex.Branch.get remoteLog
