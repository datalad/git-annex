{- git-annex scheduled activities
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.ScheduledActivity where

import Common
import Utility.Scheduled
import Utility.HumanTime
import Types.UUID

data ScheduledActivity 
	= ScheduledSelfFsck Schedule Duration
	| ScheduledRemoteFsck UUID Schedule Duration
  deriving (Eq, Read, Show, Ord)

getSchedule :: ScheduledActivity -> Schedule
getSchedule (ScheduledSelfFsck s _) = s
getSchedule (ScheduledRemoteFsck _ s _) = s

getDuration :: ScheduledActivity -> Duration
getDuration (ScheduledSelfFsck _ d) = d
getDuration (ScheduledRemoteFsck _ _ d) = d

fromScheduledActivity :: ScheduledActivity -> String
fromScheduledActivity (ScheduledSelfFsck s d) = unwords
	[ "fsck self", fromDuration d, fromSchedule s ]
fromScheduledActivity (ScheduledRemoteFsck u s d) = unwords
	[ "fsck", fromUUID u, fromDuration d, fromSchedule s ]

toScheduledActivity :: String -> Maybe ScheduledActivity
toScheduledActivity = eitherToMaybe . parseScheduledActivity

parseScheduledActivity :: String -> Either String ScheduledActivity
parseScheduledActivity s = case words s of
	("fsck":"self":d:rest) -> qualified $ ScheduledSelfFsck
		<$> parseSchedule (unwords rest)
		<*> getduration d
	("fsck":u:d:rest) -> qualified $ ScheduledRemoteFsck
		<$> pure (toUUID u)
		<*> parseSchedule (unwords rest)
		<*> getduration d
	_ -> qualified $ Left "unknown activity"
  where
	qualified (Left e) = Left $ e ++ " in \"" ++ s ++ "\""
	qualified v = v
	getduration d = maybe (Left $ "failed to parse duration \""++d++"\"") Right (parseDuration d)
