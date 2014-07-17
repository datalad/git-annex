{- git-annex branch transitions
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Branch.Transitions (
	FileTransition(..),
	getTransitionCalculator
) where

import Logs
import Logs.Transitions
import Logs.UUIDBased as UUIDBased
import Logs.Presence.Pure as Presence
import Types.TrustLevel
import Types.UUID

import qualified Data.Map as M

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
		| otherwise -> ChangeFile $ UUIDBased.showLog id $ dropDeadFromUUIDBasedLog trustmap $ UUIDBased.parseLog Just content
	Just NewUUIDBasedLog -> ChangeFile $
		UUIDBased.showLogNew id $ dropDeadFromUUIDBasedLog trustmap $ UUIDBased.parseLogNew Just content
	Just (PresenceLog _) ->
		let newlog = Presence.compactLog $ dropDeadFromPresenceLog trustmap $ Presence.parseLog content
		in if null newlog
			then RemoveFile
			else ChangeFile $ Presence.showLog newlog
	Just OtherLog -> PreserveFile
	Nothing -> PreserveFile

dropDeadFromUUIDBasedLog :: TrustMap -> UUIDBased.Log String -> UUIDBased.Log String
dropDeadFromUUIDBasedLog trustmap = M.filterWithKey $ notDead trustmap . const

{- Presence logs can contain UUIDs or other values. Any line that matches
 - a dead uuid is dropped; any other values are passed through. -}
dropDeadFromPresenceLog :: TrustMap -> [Presence.LogLine] -> [Presence.LogLine]
dropDeadFromPresenceLog trustmap = filter $ notDead trustmap (toUUID . Presence.info)

notDead :: TrustMap -> (v -> UUID) -> v -> Bool
notDead trustmap a v = M.findWithDefault SemiTrusted (a v) trustmap /= DeadTrusted
