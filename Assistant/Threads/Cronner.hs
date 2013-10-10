{- git-annex assistant sceduled jobs runner
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable, CPP #-}

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
import Assistant.TransferQueue
import Annex.Content
import Logs.Transfer
import Assistant.Types.UrlRenderer
import Assistant.Alert
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
#endif

import Control.Concurrent.Async
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Exception as E
import qualified Data.Text as T

{- Loads schedules for this repository, and fires off one thread for each 
 - scheduled event. These threads sleep until the next time the event
 - should run.
 -
 - In the meantime the main thread waits for any changes to the
 - schedules. When there's a change, compare the old and new list of
 - schedules to find deleted and added ones. Start new threads for added
 - ones, and kill the threads for deleted ones. -}
cronnerThread :: UrlRenderer -> NamedThread
cronnerThread urlrenderer = namedThreadUnchecked "Cronner" $ do
	dstatus <- getDaemonStatus
	h <- liftIO $ newNotificationHandle False (scheduleLogNotifier dstatus)
	go h M.empty
  where
	go h m = do
		activities <- liftAnnex $ scheduleGet =<< getUUID

		let addedactivities = activities `S.difference` M.keysSet m
		let removedactivities = M.keysSet m `S.difference` activities

		forM_ (S.toList removedactivities) $ \activity ->
			case M.lookup activity m of
				Just a -> do
					debug ["stopping removed job for", fromScheduledActivity activity, show (asyncThreadId a)]
					liftIO $ cancel a
				Nothing -> noop

		lastruntimes <- liftAnnex getLastRunTimes
		addedm <- M.fromList <$> startactivities (S.toList addedactivities) lastruntimes

		liftIO $ waitNotification h
		debug ["reloading changed activities"]

		let m' = M.difference (M.union addedm m)
			(M.filterWithKey (\k _ -> S.member k removedactivities) m)
		go h m'
  	startactivities as lastruntimes = forM as $ \activity -> do
		runner <- asIO2 (activityThread urlrenderer)
		a <- liftIO $ async $
			runner activity (M.lookup activity lastruntimes)
		return (activity, a)

{- Calculate the next time the activity is scheduled to run, then
 - sleep until that time, and run it. Then call setLastRunTime, and
 - loop.
 -}
activityThread :: UrlRenderer -> ScheduledActivity -> Maybe LocalTime -> Assistant ()
activityThread urlrenderer activity lasttime = go lasttime =<< getnexttime lasttime
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
		runActivity urlrenderer activity
		debug ["finished", desc]
		liftAnnex $ setLastRunTime activity nowt
		go (Just nowt) =<< getnexttime (Just nowt)

secondsUntilLocalTime :: LocalTime -> IO Seconds
secondsUntilLocalTime t = do
	now <- getCurrentTime
	tz <- getTimeZone now
	let secs = truncate $ diffUTCTime (localTimeToUTC tz t) now
	return $ if secs > 0
		then Seconds secs
		else Seconds 0

runActivity :: UrlRenderer -> ScheduledActivity -> Assistant ()
runActivity urlrenderer (ScheduledSelfFsck _ d) = do
	program <- liftIO $ readProgramFile
#ifdef WITH_WEBAPP
	button <- mkAlertButton False (T.pack "Configure") urlrenderer ConfigFsckR
	r <- alertDuring (fsckAlert button) $ liftIO $ do
		E.try (runfsck program) :: IO (Either E.SomeException ExitCode)
	either (liftIO . E.throwIO) (const noop) r
#else
	runfsck program
#endif
	queueBad
  where
  	runfsck program = niceShell $
		program ++ " fsck --incremental-schedule=1d --time-limit=" ++ fromDuration d

runActivity _ (ScheduledRemoteFsck _ _ _) =
	debug ["remote fsck not implemented yet"]

queueBad :: Assistant ()
queueBad = mapM_ queue =<< liftAnnex (dirKeys gitAnnexBadDir)
  where
	queue k = queueTransfers "fsck found bad file; redownloading" Next k Nothing Download

{- Runs a shell command niced, until it terminates.
 - 
 - When an async exception is received, the command is sent a SIGTERM,
 - and after it finishes shutting down the exception is re-raised.  -}
niceShell :: String -> IO ExitCode
niceShell command = do
	(_, _, _, pid) <- createProcess $ proc "sh"
		[ "-c"
		, "exec " ++ nicedcommand
		]
	r <- E.try (waitForProcess pid) :: IO (Either E.SomeException ExitCode)
	case r of
		Right exitcode -> return exitcode
		Left asyncexception -> do
			terminateProcess pid
			void $ waitForProcess pid
			E.throwIO asyncexception
  where
  	nicedcommand
		| Build.SysConfig.nice = "nice " ++ command
		| otherwise = command
