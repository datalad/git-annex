{- git-annex assistant sceduled jobs runner
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Assistant.Threads.Cronner (
	cronnerThread
) where

import Assistant.Common
import Assistant.DaemonStatus
import Utility.NotificationBroadcaster
import Annex.UUID
import Config.Files
import Logs.Schedule
import Utility.Scheduled
import Types.ScheduledActivity
import Utility.ThreadScheduler
import Utility.HumanTime
import qualified Build.SysConfig

import Control.Concurrent.Async
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Exception as E
import Data.Typeable

data ActivityException = PleaseTerminate
	deriving (Typeable, Show)

instance E.Exception ActivityException

{- Loads schedules for this repository, and fires off one thread for each 
 - scheduled event. These threads sleep until the next time the event
 - should run.
 -
 - In the meantime the main thread waits for any changes to the
 - schedules. When there's a change, compare the old and new list of
 - schedules to find deleted and added ones. Start new threads for added
 - ones, and send the threads a PleaseTerminate exception for the deleted
 - ones. -}
cronnerThread :: NamedThread
cronnerThread = namedThreadUnchecked "Cronner" $ do
	dstatus <- getDaemonStatus
	h <- liftIO $ newNotificationHandle False (scheduleLogNotifier dstatus)
	go h M.empty
  where
	go h m = do
		activities <- liftAnnex $ scheduleGet =<< getUUID

		let addedactivities = activities `S.difference` M.keysSet m
		let removedactivities = M.keysSet m `S.difference` activities

		liftIO $ forM_ (mapMaybe (`M.lookup` m) $ S.toList removedactivities) $ 
			flip cancelWith PleaseTerminate

		lastruntimes <- liftAnnex getLastRunTimes
		addedm <- M.fromList <$> startactivities (S.toList addedactivities) lastruntimes

		liftIO $ waitNotification h

		let m' = M.difference (M.union addedm m)
			(M.filterWithKey (\k _ -> S.member k removedactivities) m)
		go h m'
  	startactivities as lastruntimes = forM as $ \activity -> do
		runner <- asIO2 activityThread
		a <- liftIO $ async $
			runner activity (M.lookup activity lastruntimes)
		return (activity, a)

{- Calculate the next time the activity is scheduled to run, then
 - sleep until that time, and run it. Then call setLastRunTime, and
 - loop.
 -
 - At any point, a PleaseTerminate could be received. This should result in
 - the thread and any processes it has run shutting down.
 -}
activityThread :: ScheduledActivity -> Maybe LocalTime -> Assistant ()
activityThread activity lasttime = go lasttime =<< getnexttime lasttime
  where
  	getnexttime = liftIO . nextTime schedule
  	go _ Nothing = debug ["no scheduled events left for", desc]
	go l (Just (NextTimeExactly t)) = runafter l t Nothing run
	go l (Just (NextTimeWindow windowstart windowend)) =
		runafter l windowstart (Just windowend) run
	desc = fromScheduledActivity activity
	schedule = getSchedule activity
	runafter l t mmaxt a = do
		seconds <- liftIO $ secondsUntilLocalTime t
		when (seconds > Seconds 0) $ do
			debug ["waiting", show seconds, "for next scheduled", desc]
			liftIO $ threadDelaySeconds seconds
		now <- liftIO getCurrentTime
		tz <- liftIO $ getTimeZone now
		let nowt = utcToLocalTime tz now
		if tolate nowt tz
			then do
				debug ["too late to run scheduled", desc]
				go l =<< getnexttime l
			else a nowt
	  where
	  	tolate nowt tz = case mmaxt of
			Just maxt -> nowt > maxt
			-- allow the job to start 10 minutes late
			Nothing ->diffUTCTime 
				(localTimeToUTC tz nowt)
				(localTimeToUTC tz t) > 600
	run nowt = do
		debug ["starting", desc]
		runActivity activity
		debug ["finished", desc]
		liftAnnex $ setLastRunTime activity nowt
		go (Just nowt) =<< getnexttime (Just nowt)

secondsUntilLocalTime :: LocalTime -> IO Seconds
secondsUntilLocalTime t = do
	now <- getCurrentTime
	tz <- getTimeZone now
	let secs = truncate $ diffUTCTime now (localTimeToUTC tz t)
	return $ if secs > 0
		then Seconds secs
		else Seconds 0

runActivity :: ScheduledActivity -> Assistant ()
runActivity (ScheduledSelfFsck _ d) = do
	program <- liftIO $ readProgramFile
	void $ liftIO $ niceShell $
		program ++ " fsck --incremental-schedule=1d --time-limit=" ++ fromDuration d
runActivity (ScheduledRemoteFsck _ _ _) =
	debug ["remote fsck not implemented yet"]

niceShell :: String -> IO Bool
niceShell command = boolSystem "sh"
	[ Param "-c"
	, Param nicedcommand
	]
  where
  	nicedcommand
		| Build.SysConfig.nice = "nice " ++ command
		| otherwise = command
