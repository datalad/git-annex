{- git-annex uuid log
 -
 - uuid.log stores a list of known uuids, and their descriptions.
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.UUID (
	uuidLog,
	describeUUID,
	uuidDescMap,
	uuidDescMapLoad,
	uuidDescMapRaw,
) where

import Types.UUID
import Annex.Common
import Annex.VectorClock
import qualified Annex
import qualified Annex.Branch
import Logs
import Logs.UUIDBased
import qualified Annex.UUID

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A

{- Records a description for a uuid in the log. -}
describeUUID :: UUID -> UUIDDesc -> Annex ()
describeUUID uuid desc = do
	c <- currentVectorClock
	Annex.Branch.change uuidLog $
		buildLogOld buildUUIDDesc . changeLog c uuid desc . parseUUIDLog

{- The map is cached for speed. -}
uuidDescMap :: Annex UUIDDescMap
uuidDescMap = maybe uuidDescMapLoad return =<< Annex.getState Annex.uuiddescmap

{- Read the uuidLog into a map, and cache it for later use.
 -
 - If the current repository has not been described, it is still included
 - in the map with an empty description. -}
uuidDescMapLoad :: Annex UUIDDescMap
uuidDescMapLoad = do
	m <- uuidDescMapRaw
	u <- Annex.UUID.getUUID
	let m' = M.insertWith preferold u mempty m
	Annex.changeState $ \s -> s { Annex.uuiddescmap = Just m' }
	return m'
  where
	preferold = flip const

{- Read the uuidLog into a map. Includes only actually set descriptions. -}
uuidDescMapRaw :: Annex UUIDDescMap
uuidDescMapRaw = simpleMap . parseUUIDLog <$> Annex.Branch.get uuidLog

parseUUIDLog :: L.ByteString -> Log UUIDDesc
parseUUIDLog = parseLogOld (UUIDDesc <$> A.takeByteString)
