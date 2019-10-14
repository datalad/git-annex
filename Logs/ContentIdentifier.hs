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
import Types.Import
import Types.RemoteState
import qualified Annex.Branch
import Logs.ContentIdentifier.Pure as X
import qualified Annex

import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

-- | Records a remote's content identifier and the key that it corresponds to.
--
-- A remote may use multiple content identifiers for the same key over time,
-- so ones that were recorded before are preserved.
recordContentIdentifier :: RemoteStateHandle -> ContentIdentifier -> Key -> Annex ()
recordContentIdentifier (RemoteStateHandle u) cid k = do
	c <- liftIO currentVectorClock
	config <- Annex.getGitConfig
	Annex.Branch.change (remoteContentIdentifierLogFile config k) $
		buildLog . addcid c . parseLog
  where
	addcid c l = changeMapLog c u (cid :| contentIdentifierList (M.lookup u m)) l
	  where
		m = simpleMap l

-- | Get all known content identifiers for a key.
getContentIdentifiers :: Key -> Annex [(RemoteStateHandle, [ContentIdentifier])]
getContentIdentifiers k = do
	config <- Annex.getGitConfig
	map (\(u, l) -> (RemoteStateHandle u, NonEmpty.toList l) )
		. M.toList . simpleMap . parseLog
		<$> Annex.Branch.get (remoteContentIdentifierLogFile config k)
