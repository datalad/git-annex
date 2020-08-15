{- git-annex scheduled activities
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.ScheduledActivity where

import Common
import Utility.Scheduled
import Utility.HumanTime
import Types.UUID

import Data.Either

data ScheduledActivity 
	= ScheduledSelfFsck Schedule Duration
	| ScheduledRemoteFsck UUID Schedule Duration
	deriving (Eq, Read, Show, Ord)

{- Activities that run on a remote, within a time window, so
 - should be run when the remote gets connected. -}
connectActivityUUID :: ScheduledActivity -> Maybe UUID
connectActivityUUID (ScheduledRemoteFsck u (Schedule _ AnyTime) _) = Just u
connectActivityUUID _ = Nothing

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
		<*> parseDuration d
	("fsck":u:d:rest) -> qualified $ ScheduledRemoteFsck
		<$> pure (toUUID u)
		<*> parseSchedule (unwords rest)
		<*> parseDuration d
	_ -> qualified $ Left "unknown activity"
  where
	qualified (Left e) = Left $ e ++ " in \"" ++ s ++ "\""
	qualified v = v

fromScheduledActivities :: [ScheduledActivity] -> String
fromScheduledActivities = intercalate "; " . map fromScheduledActivity

parseScheduledActivities :: String -> Either String [ScheduledActivity]
parseScheduledActivities s
	| null bad = Right good
	| otherwise = Left $ intercalate "; " bad
  where
	(bad, good) = partitionEithers $
		map parseScheduledActivity $ split "; " s
