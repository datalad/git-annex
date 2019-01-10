{- git-annex uuid log
 -
 - uuid.log stores a list of known uuids, and their descriptions.
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.UUID (
	uuidLog,
	describeUUID,
	uuidDescMap,
	uuidDescMapLoad
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
	c <- liftIO currentVectorClock
	Annex.Branch.change uuidLog $
		buildLog buildUUIDDesc . changeLog c uuid desc . parseUUIDLog

{- The map is cached for speed. -}
uuidDescMap :: Annex UUIDDescMap
uuidDescMap = maybe uuidDescMapLoad return =<< Annex.getState Annex.uuiddescmap

{- Read the uuidLog into a simple Map.
 -
 - The UUID of the current repository is included explicitly, since
 - it may not have been described and otherwise would not appear. -}
uuidDescMapLoad :: Annex UUIDDescMap
uuidDescMapLoad = do
	m <- simpleMap . parseUUIDLog <$> Annex.Branch.get uuidLog
	u <- Annex.UUID.getUUID
	let m' = M.insertWith preferold u mempty m
	Annex.changeState $ \s -> s { Annex.uuiddescmap = Just m' }
	return m'
  where
	preferold = flip const

parseUUIDLog :: L.ByteString -> Log UUIDDesc
parseUUIDLog = parseLog (UUIDDesc <$> A.takeByteString)
