{- git-annex scheduled activities
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.ScheduledActivity where

import Common
import Utility.Scheduled
import Types.UUID

data ScheduledActivity 
	= ScheduledSelfFsck Schedule 
	| ScheduledRemoteFsck UUID Schedule

fromScheduledActivity :: ScheduledActivity -> String
fromScheduledActivity (ScheduledSelfFsck s) = 
	"fsck self at " ++ fromSchedule s
fromScheduledActivity (ScheduledRemoteFsck u s) = 
	"fsck " ++ fromUUID u ++ " at " ++ fromSchedule s

toScheduledActivity :: String -> Maybe ScheduledActivity
toScheduledActivity s = case words s of
	("fsck":"self":rest) -> ScheduledSelfFsck
		<$> toSchedule (unwords rest)
	("fsck":u:rest) -> ScheduledRemoteFsck
		<$> pure (toUUID u)
		<*> toSchedule (unwords rest)
	_ -> Nothing
