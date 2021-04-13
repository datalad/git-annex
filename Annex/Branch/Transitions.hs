{- git-annex branch transitions
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import qualified Logs.Remote.Pure as Remote
import Types.TrustLevel
import Types.UUID
import Types.MetaData
import Types.Remote
import Types.GitConfig (GitConfig)
import Types.ProposedAccepted
import Annex.SpecialRemote.Config

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

data FileTransition
	= ChangeFile Builder
	| PreserveFile

type TransitionCalculator = GitConfig -> TrustMap -> M.Map UUID RemoteConfig -> RawFilePath -> L.ByteString -> FileTransition

getTransitionCalculator :: Transition -> Maybe TransitionCalculator
getTransitionCalculator ForgetGitHistory = Nothing
getTransitionCalculator ForgetDeadRemotes = Just dropDead

-- Removes data about all dead repos.
--
-- The trust log is not changed, because other, unmerged clones
-- may contain other data about the dead repos. So we need to remember
-- which are dead to later remove that.
--
-- When the remote log contains a sameas-uuid pointing to a dead uuid,
-- the uuid of that remote configuration is also effectively dead,
-- though not in the trust log. There may be per-remote state stored using
-- the latter uuid, that also needs to be removed. The sameas-uuid
-- is not removed from the remote log, for the same reason the trust log
-- is not changed.
dropDead :: TransitionCalculator
dropDead gc trustmap remoteconfigmap f content = case getLogVariety gc f of
	Just OldUUIDBasedLog
		| f == trustLog -> PreserveFile
		| f == remoteLog -> ChangeFile $
			Remote.buildRemoteConfigLog $
				M.mapWithKey minimizesameasdead $
					dropDeadFromMapLog trustmap id $
						Remote.parseRemoteConfigLog content
		| otherwise -> ChangeFile $
			UUIDBased.buildLogOld byteString $
				dropDeadFromMapLog trustmap' id $
					UUIDBased.parseLogOld A.takeByteString content
	Just NewUUIDBasedLog -> ChangeFile $
		UUIDBased.buildLogNew byteString $
			dropDeadFromMapLog trustmap' id $
				UUIDBased.parseLogNew A.takeByteString content
	Just (ChunkLog _) -> ChangeFile $
		Chunk.buildLog $ dropDeadFromMapLog trustmap' fst $
			Chunk.parseLog content
	Just (PresenceLog _) -> ChangeFile $ Presence.buildLog $
		Presence.compactLog $
			dropDeadFromPresenceLog trustmap' $
				Presence.parseLog content
	Just RemoteMetaDataLog -> ChangeFile $ MetaData.buildLog $
		dropDeadFromRemoteMetaDataLog trustmap' $
			MetaData.simplifyLog $ MetaData.parseLog content
	Just OtherLog -> PreserveFile
	Nothing -> PreserveFile
  where
	trustmap' = trustmap `M.union`
		M.map (const DeadTrusted) (M.filter sameasdead remoteconfigmap)
	sameasdead cm =
		case toUUID . fromProposedAccepted <$> M.lookup sameasUUIDField cm of
			Nothing -> False
			Just u' -> M.lookup u' trustmap == Just DeadTrusted
	minimizesameasdead u l
		| M.lookup u trustmap' == Just DeadTrusted =
			l { UUIDBased.value = minimizesameasdead' (UUIDBased.value l) }
		| otherwise = l
	minimizesameasdead' c = M.restrictKeys c (S.singleton sameasUUIDField)

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
