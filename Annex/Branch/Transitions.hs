{- git-annex branch transitions
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Branch.Transitions (
	FileTransition(..),
	getTransitionCalculator
) where

import Common
import Logs
import Logs.Transitions
import qualified Logs.UUIDBased as UUIDBased
import qualified Logs.Presence.Pure as Presence
import qualified Logs.Chunk.Pure as Chunk
import qualified Logs.MetaData.Pure as MetaData
import Types.TrustLevel
import Types.UUID
import Types.MetaData

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

data FileTransition
	= ChangeFile L.ByteString
	| RemoveFile
	| PreserveFile

type TransitionCalculator = FilePath -> L.ByteString -> TrustMap -> FileTransition

getTransitionCalculator :: Transition -> Maybe TransitionCalculator
getTransitionCalculator ForgetGitHistory = Nothing
getTransitionCalculator ForgetDeadRemotes = Just dropDead

dropDead :: FilePath -> L.ByteString -> TrustMap -> FileTransition
dropDead f content trustmap = case getLogVariety f of
	Just UUIDBasedLog
		-- Don't remove the dead repo from the trust log,
		-- because git remotes may still exist, and they need
		-- to still know it's dead.
		| f == trustLog -> PreserveFile
		| otherwise -> ChangeFile $ toLazyByteString $
			UUIDBased.buildLog (byteString . encodeBS) $
				dropDeadFromMapLog trustmap id $ UUIDBased.parseLog Just (decodeBL content)
	Just NewUUIDBasedLog -> ChangeFile $ toLazyByteString $
		UUIDBased.buildLogNew (byteString . encodeBS) $
			dropDeadFromMapLog trustmap id $
				UUIDBased.parseLogNew Just (decodeBL content)
	Just (ChunkLog _) -> ChangeFile $ toLazyByteString $
		Chunk.buildLog $ dropDeadFromMapLog trustmap fst $ Chunk.parseLog (decodeBL content)
	Just (PresenceLog _) ->
		let newlog = Presence.compactLog $ dropDeadFromPresenceLog trustmap $ Presence.parseLog content
		in if null newlog
			then RemoveFile
			else ChangeFile $ toLazyByteString $ Presence.buildLog newlog
	Just RemoteMetaDataLog ->
		let newlog = dropDeadFromRemoteMetaDataLog trustmap $ MetaData.simplifyLog $ MetaData.parseLog content
		in if S.null newlog
			then RemoveFile
			else ChangeFile $ toLazyByteString $ MetaData.buildLog newlog
	Just OtherLog -> PreserveFile
	Nothing -> PreserveFile

dropDeadFromMapLog :: TrustMap -> (k -> UUID) -> M.Map k v -> M.Map k v
dropDeadFromMapLog trustmap getuuid =
	M.filterWithKey $ \k _v -> notDead trustmap getuuid k

{- Presence logs can contain UUIDs or other values. Any line that matches
 - a dead uuid is dropped; any other values are passed through. -}
dropDeadFromPresenceLog :: TrustMap -> [Presence.LogLine] -> [Presence.LogLine]
dropDeadFromPresenceLog trustmap =
	filter $ notDead trustmap (toUUID . Presence.fromLogInfo . Presence.info)

dropDeadFromRemoteMetaDataLog :: TrustMap -> MetaData.Log MetaData -> MetaData.Log MetaData
dropDeadFromRemoteMetaDataLog trustmap =
	MetaData.filterOutEmpty . MetaData.filterRemoteMetaData (notDead trustmap id)

notDead :: TrustMap -> (v -> UUID) -> v -> Bool
notDead trustmap a v = M.findWithDefault def (a v) trustmap /= DeadTrusted
