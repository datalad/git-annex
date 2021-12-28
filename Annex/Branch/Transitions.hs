{- git-annex branch transitions
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Branch.Transitions (
	getTransitionCalculator,
	filterBranch,
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
import Types.Transitions
import Types.GitConfig (GitConfig)
import Types.ProposedAccepted
import Annex.SpecialRemote.Config

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

getTransitionCalculator :: Transition -> Maybe (TrustMap -> M.Map UUID RemoteConfig -> GitConfig -> TransitionCalculator)
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
dropDead :: TrustMap -> M.Map UUID RemoteConfig -> GitConfig -> TransitionCalculator
dropDead trustmap remoteconfigmap gc f content
	| f == trustLog = PreserveFile
	| f == remoteLog = ChangeFile $
		Remote.buildRemoteConfigLog $
			M.mapWithKey minimizesameasdead $
				filterMapLog (notdead trustmap) id $
					Remote.parseRemoteConfigLog content
	| otherwise = filterBranch (notdead trustmap') gc f content
  where
	notdead m u = M.findWithDefault def u m /= DeadTrusted
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

filterBranch :: (UUID -> Bool) -> GitConfig -> TransitionCalculator
filterBranch wantuuid gc f content = case getLogVariety gc f of
	Just OldUUIDBasedLog -> ChangeFile $
		UUIDBased.buildLogOld byteString $
			filterMapLog wantuuid id $
				UUIDBased.parseLogOld A.takeByteString content
	Just NewUUIDBasedLog -> ChangeFile $
		UUIDBased.buildLogNew byteString $
			filterMapLog wantuuid id $
				UUIDBased.parseLogNew A.takeByteString content
	Just (ChunkLog _) -> ChangeFile $
		Chunk.buildLog $ filterMapLog wantuuid fst $
			Chunk.parseLog content
	Just (LocationLog _) -> ChangeFile $ Presence.buildLog $
		Presence.compactLog $
			filterLocationLog wantuuid $
				Presence.parseLog content
	Just (UrlLog _) -> PreserveFile
	Just RemoteMetaDataLog -> ChangeFile $ MetaData.buildLog $
		filterRemoteMetaDataLog wantuuid $
			MetaData.simplifyLog $ MetaData.parseLog content
	Just OtherLog -> PreserveFile
	Nothing -> PreserveFile

filterMapLog :: (UUID -> Bool) -> (k -> UUID) -> M.Map k v -> M.Map k v
filterMapLog wantuuid getuuid = M.filterWithKey $ \k _v -> wantuuid (getuuid k)

filterLocationLog :: (UUID -> Bool) -> [Presence.LogLine] -> [Presence.LogLine]
filterLocationLog wantuuid = filter $
	wantuuid . toUUID . Presence.fromLogInfo . Presence.info

filterRemoteMetaDataLog :: (UUID -> Bool) -> MetaData.Log MetaData -> MetaData.Log MetaData
filterRemoteMetaDataLog wantuuid = 
	MetaData.filterOutEmpty . MetaData.filterRemoteMetaData wantuuid
