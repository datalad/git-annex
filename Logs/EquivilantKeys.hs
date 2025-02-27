{- Logs listing keys that are equivalent to a key.
 -
 - Copyright 2024-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Logs.EquivilantKeys (
	getEquivilantKeys,
	setEquivilantKey,
	updateEquivilantKeys,
	generateEquivilantKey,
) where

import Annex.Common
import qualified Annex
import Logs
import Logs.Presence
import qualified Annex.Branch
import qualified Backend.Hash
import Types.KeySource
import Types.Backend
import Types.Remote (Verification(..))
import Utility.Metered

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

-- The Backend must use a cryptographically secure hash.
--
-- This returns Verified when when an equivilant key has been added to the
-- log (or was already in the log). This is to avoid hashing the object
-- again later.
updateEquivilantKeys :: Backend -> OsPath -> Key -> [Key] -> Annex (Maybe Verification)
updateEquivilantKeys b obj key eks = generateEquivilantKey b obj >>= \case
	Nothing -> return Nothing
	Just ek -> do
		unless (ek `elem` eks) $
			setEquivilantKey key ek
		return (Just Verified)

generateEquivilantKey :: Backend -> OsPath -> Annex (Maybe Key)
generateEquivilantKey b obj =
	case genKey b of
		Just genkey -> do
			showSideAction (UnquotedString Backend.Hash.descChecksum)
			Just <$> genkey source nullMeterUpdate
		Nothing -> return Nothing
  where
	source = KeySource
		{ keyFilename = mempty -- avoid adding any extension
		, contentLocation = obj
		, inodeCache = Nothing
		}
