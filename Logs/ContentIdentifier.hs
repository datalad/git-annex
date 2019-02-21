{- Remote content identifier logs.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.ContentIdentifier (
	module X,
	recordContentIdentifier,
	getContentIdentifiers,
) where

import Annex.Common
import Logs
import Logs.MapLog
import Types.Remote (ContentIdentifier)
import qualified Annex.Branch
import Logs.ContentIdentifier.Pure as X
import qualified Annex

import qualified Data.Map as M

-- | Records a remote's content identifier and the key that it corresponds to.
--
-- A remote may use multiple content identifiers for the same key over time,
-- so ones that were recorded before are preserved.
recordContentIdentifier :: UUID -> ContentIdentifier -> Key -> Annex ()
recordContentIdentifier u cid k = do
	c <- liftIO currentVectorClock
	config <- Annex.getGitConfig
	Annex.Branch.change (remoteContentIdentifierLogFile config k) $
		buildLog . addcid c . parseLog
  where
	addcid c l = changeMapLog c u (cid:fromMaybe [] (M.lookup u m)) l
	  where
		m = simpleMap l

-- | Get all content identifiers that a remote is known to use for a key.
getContentIdentifiers :: UUID -> Key -> Annex [ContentIdentifier]
getContentIdentifiers u k = do
	config <- Annex.getGitConfig
	fromMaybe [] . M.lookup u . simpleMap . parseLog
		<$> Annex.Branch.get (remoteContentIdentifierLogFile config k)
