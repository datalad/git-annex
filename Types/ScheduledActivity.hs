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
  deriving (Eq, Read, Show, Ord)

fromScheduledActivity :: ScheduledActivity -> String
fromScheduledActivity (ScheduledSelfFsck s) = 
	"fsck self " ++ fromSchedule s
fromScheduledActivity (ScheduledRemoteFsck u s) = 
	"fsck " ++ fromUUID u ++ fromSchedule s

toScheduledActivity :: String -> Maybe ScheduledActivity
toScheduledActivity = eitherToMaybe . parseScheduledActivity

parseScheduledActivity :: String -> Either String ScheduledActivity
parseScheduledActivity s = case words s of
	("fsck":"self":rest) -> qualified $ ScheduledSelfFsck
		<$> parseSchedule (unwords rest)
	("fsck":u:rest) -> qualified $ ScheduledRemoteFsck
		<$> pure (toUUID u)
		<*> parseSchedule (unwords rest)
	_ -> qualified $ Left "unknown activity"
  where
	qualified (Left e) = Left $ e ++ " in \"" ++ s ++ "\""
	qualified v = v
