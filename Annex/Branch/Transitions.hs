{- git-annex branch transitions
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Branch.Transitions (
	FileTransition(..),
	getTransitionCalculator
) where

import Logs
import Logs.Transitions
import qualified Logs.UUIDBased as UUIDBased
import qualified Logs.Presence.Pure as Presence
import qualified Logs.Chunk.Pure as Chunk
import Types.TrustLevel
import Types.UUID

import qualified Data.Map as M
import Data.Default

data FileTransition
	= ChangeFile String
	| RemoveFile
	| PreserveFile

type TransitionCalculator = FilePath -> String -> TrustMap -> FileTransition

getTransitionCalculator :: Transition -> Maybe TransitionCalculator
getTransitionCalculator ForgetGitHistory = Nothing
getTransitionCalculator ForgetDeadRemotes = Just dropDead

dropDead :: FilePath -> String -> TrustMap -> FileTransition
dropDead f content trustmap = case getLogVariety f of
	Just UUIDBasedLog
		-- Don't remove the dead repo from the trust log,
		-- because git remotes may still exist, and they need
		-- to still know it's dead.
		| f == trustLog -> PreserveFile
		| otherwise -> ChangeFile $ UUIDBased.showLog id $ dropDeadFromMapLog trustmap id $ UUIDBased.parseLog Just content
	Just NewUUIDBasedLog -> ChangeFile $
		UUIDBased.showLogNew id $ dropDeadFromMapLog trustmap id $ UUIDBased.parseLogNew Just content
	Just (ChunkLog _) -> ChangeFile $
		Chunk.showLog $ dropDeadFromMapLog trustmap fst $ Chunk.parseLog content
	Just (PresenceLog _) ->
		let newlog = Presence.compactLog $ dropDeadFromPresenceLog trustmap $ Presence.parseLog content
		in if null newlog
			then RemoveFile
			else ChangeFile $ Presence.showLog newlog
	Just OtherLog -> PreserveFile
	Nothing -> PreserveFile

dropDeadFromMapLog :: TrustMap -> (k -> UUID) -> M.Map k v -> M.Map k v
dropDeadFromMapLog trustmap getuuid = M.filterWithKey $ \k _v -> notDead trustmap getuuid k

{- Presence logs can contain UUIDs or other values. Any line that matches
 - a dead uuid is dropped; any other values are passed through. -}
dropDeadFromPresenceLog :: TrustMap -> [Presence.LogLine] -> [Presence.LogLine]
dropDeadFromPresenceLog trustmap = filter $ notDead trustmap (toUUID . Presence.info)

notDead :: TrustMap -> (v -> UUID) -> v -> Bool
notDead trustmap a v = M.findWithDefault def (a v) trustmap /= DeadTrusted
