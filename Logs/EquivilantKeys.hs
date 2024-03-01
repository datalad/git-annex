{- Logs listing keys that are equivilant to a key.
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Logs.EquivilantKeys (
	getEquivilantKeys,
	setEquivilantKey,
) where

import Annex.Common
import qualified Annex
import Logs
import Logs.Presence
import qualified Annex.Branch

getEquivilantKeys :: Key -> Annex [Key]
getEquivilantKeys key = do
	config <- Annex.getGitConfig
	nub . mapMaybe (deserializeKey' . fromLogInfo)
		<$> presentLogInfo (equivilantKeysLogFile config key)

setEquivilantKey :: Key -> Key -> Annex ()
setEquivilantKey key equivkey = do
	config <- Annex.getGitConfig
	addLog (Annex.Branch.RegardingUUID []) (equivilantKeysLogFile config key)
		InfoPresent (LogInfo (serializeKey' equivkey))
