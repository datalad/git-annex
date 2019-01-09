{- git-annex scheduled activities log
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Schedule (
	scheduleLog,
	scheduleSet,
	scheduleAdd,
	scheduleRemove,
	scheduleChange,
	scheduleGet,
	scheduleMap,
	getLastRunTimes,
	setLastRunTime,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.LocalTime
import Data.ByteString.Builder

import Annex.Common
import Types.ScheduledActivity
import qualified Annex.Branch
import Logs
import Logs.UUIDBased
import Logs.File

scheduleSet :: UUID -> [ScheduledActivity] -> Annex ()
scheduleSet uuid@(UUID _) activities = do
	c <- liftIO currentVectorClock
	Annex.Branch.change scheduleLog $
		buildLog (byteString . encodeBS) 
			. changeLog c uuid val 
			. parseLog Just . decodeBL
  where
	val = fromScheduledActivities activities
scheduleSet NoUUID _ = error "unknown UUID; cannot modify"

scheduleMap :: Annex (M.Map UUID [ScheduledActivity])
scheduleMap = simpleMap
	. parseLogWithUUID parser . decodeBL
	<$> Annex.Branch.get scheduleLog
  where
	parser _uuid = eitherToMaybe . parseScheduledActivities

scheduleGet :: UUID -> Annex (S.Set ScheduledActivity)
scheduleGet u = do
	m <- scheduleMap
	return $ maybe S.empty S.fromList (M.lookup u m)

scheduleRemove :: UUID -> ScheduledActivity -> Annex ()
scheduleRemove u activity = scheduleChange u $ S.delete activity

scheduleAdd :: UUID -> ScheduledActivity -> Annex ()
scheduleAdd u activity = scheduleChange u $ S.insert activity

scheduleChange :: UUID -> (S.Set ScheduledActivity -> S.Set ScheduledActivity) -> Annex ()
scheduleChange u a = scheduleSet u . S.toList . a =<< scheduleGet u

getLastRunTimes :: Annex (M.Map ScheduledActivity LocalTime)
getLastRunTimes = do
	f <- fromRepo gitAnnexScheduleState
	liftIO $ fromMaybe M.empty
		<$> catchDefaultIO Nothing (readish <$> readFile f)

setLastRunTime :: ScheduledActivity -> LocalTime -> Annex ()
setLastRunTime activity lastrun = do
	f <- fromRepo gitAnnexScheduleState
	writeLogFile f . show . M.insert activity lastrun
		=<< getLastRunTimes
