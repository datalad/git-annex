{- git-annex assistant sceduled jobs runner
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
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
import Annex.Path
import Logs.Schedule
import Utility.Scheduled
import Types.ScheduledActivity
import Utility.ThreadScheduler
import Utility.HumanTime
import Utility.Batch
import Assistant.TransferQueue
import Annex.Content
import Types.Transfer
import Assistant.Types.UrlRenderer
import Assistant.Alert
import Remote
import qualified Types.Remote as Remote
import qualified Git
import qualified Git.Fsck
import Assistant.Fsck
import Assistant.Repair

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.Set as S

{- Loads schedules for this repository, and fires off one thread for each 
 - scheduled event that runs on this repository. Each thread sleeps until
 - its event is scheduled to run.
 -
 - To handle events that run on remotes, which need to only run when
 - their remote gets connected, threads are also started, and are passed
 - a MVar to wait on, which is stored in the DaemonStatus's
 - connectRemoteNotifiers.
 -
 - In the meantime the main thread waits for any changes to the
 - schedules. When there's a change, compare the old and new list of
 - schedules to find deleted and added ones. Start new threads for added
 - ones, and kill the threads for deleted ones. -}
cronnerThread :: UrlRenderer -> NamedThread
cronnerThread urlrenderer = namedThreadUnchecked "Cronner" $ do
	fsckNudge urlrenderer Nothing
	dstatus <- getDaemonStatus
	h <- liftIO $ newNotificationHandle False (scheduleLogNotifier dstatus)
	go h M.empty M.empty
  where
	go h amap nmap = do
		activities <- liftAnnex $ scheduleGet =<< getUUID

		let addedactivities = activities `S.difference` M.keysSet amap
		let removedactivities = M.keysSet amap `S.difference` activities

		forM_ (S.toList removedactivities) $ \activity ->
			case M.lookup activity amap of
				Just a -> do
					debug ["stopping removed job for", fromScheduledActivity activity, show (asyncThreadId a)]
					liftIO $ cancel a
				Nothing -> noop

		lastruntimes <- liftAnnex getLastRunTimes
		started <- startactivities (S.toList addedactivities) lastruntimes
		let addedamap = M.fromList $ map fst started
		let addednmap = M.fromList $ catMaybes $ map snd started

		let removefiltered = M.filterWithKey (\k _ -> S.member k removedactivities)
		let amap' = M.difference (M.union addedamap amap) (removefiltered amap)
		let nmap' = M.difference (M.union addednmap nmap) (removefiltered nmap)
		modifyDaemonStatus_ $ \s -> s { connectRemoteNotifiers = M.fromListWith (++) (M.elems nmap') }

		liftIO $ waitNotification h
		debug ["reloading changed activities"]
		go h amap' nmap'
	startactivities as lastruntimes = forM as $ \activity ->
		case connectActivityUUID activity of
			Nothing -> do
				runner <- asIO2 (sleepingActivityThread urlrenderer)
				a <- liftIO $ async $
					runner activity (M.lookup activity lastruntimes)
				return ((activity, a), Nothing)
			Just u -> do
				mvar <- liftIO newEmptyMVar
				runner <- asIO2 (remoteActivityThread urlrenderer mvar)
				a <- liftIO $ async $
					runner activity (M.lookup activity lastruntimes)
				return ((activity, a), Just (activity, (u, [mvar])))

{- Calculate the next time the activity is scheduled to run, then
 - sleep until that time, and run it. Then call setLastRunTime, and
 - loop.
 -}
sleepingActivityThread :: UrlRenderer -> ScheduledActivity -> Maybe LocalTime -> Assistant ()
sleepingActivityThread urlrenderer activity lasttime = go lasttime =<< getnexttime lasttime
  where
	getnexttime = liftIO . nextTime schedule
	go _ Nothing = debug ["no scheduled events left for", desc]
	go l (Just (NextTimeExactly t)) = waitrun l t Nothing
	go l (Just (NextTimeWindow windowstart windowend)) =
		waitrun l windowstart (Just windowend)
	desc = fromScheduledActivity activity
	schedule = getSchedule activity
	waitrun l t mmaxt = do
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
			else run nowt
	  where
		tolate nowt tz = case mmaxt of
			Just maxt -> nowt > maxt
			-- allow the job to start 10 minutes late
			Nothing ->diffUTCTime 
				(localTimeToUTC tz nowt)
				(localTimeToUTC tz t) > 600
	run nowt = do
		runActivity urlrenderer activity nowt
		go (Just nowt) =<< getnexttime (Just nowt)

{- Wait for the remote to become available by waiting on the MVar.
 - Then check if the time is within a time window when activity
 - is scheduled to run, and if so run it.
 - Otherwise, just wait again on the MVar.
 -}
remoteActivityThread :: UrlRenderer -> MVar () -> ScheduledActivity -> Maybe LocalTime -> Assistant ()
remoteActivityThread urlrenderer mvar activity lasttime = do
	liftIO $ takeMVar mvar
	go =<< liftIO (nextTime (getSchedule activity) lasttime)
  where
	go (Just (NextTimeWindow windowstart windowend)) = do
		now <- liftIO getCurrentTime
		tz <- liftIO $ getTimeZone now
		if now >= localTimeToUTC tz windowstart && now <= localTimeToUTC tz windowend
			then do
				let nowt = utcToLocalTime tz now
				runActivity urlrenderer activity nowt
				loop (Just nowt)
			else loop lasttime
	go _ = noop -- running at exact time not handled here
	loop = remoteActivityThread urlrenderer mvar activity

secondsUntilLocalTime :: LocalTime -> IO Seconds
secondsUntilLocalTime t = do
	now <- getCurrentTime
	tz <- getTimeZone now
	let secs = truncate $ diffUTCTime (localTimeToUTC tz t) now
	return $ if secs > 0
		then Seconds secs
		else Seconds 0

runActivity :: UrlRenderer -> ScheduledActivity -> LocalTime -> Assistant ()
runActivity urlrenderer activity nowt = do
	debug ["starting", desc]
	runActivity' urlrenderer activity
	debug ["finished", desc]
	liftAnnex $ setLastRunTime activity nowt
  where
	desc = fromScheduledActivity activity

runActivity' :: UrlRenderer -> ScheduledActivity -> Assistant ()
runActivity' urlrenderer (ScheduledSelfFsck _ d) = do
	program <- liftIO programPath
	g <- liftAnnex gitRepo
	fsckresults <- showFscking urlrenderer Nothing $ tryNonAsync $ do
		void $ batchCommand program (Param "fsck" : annexFsckParams d)
		Git.Fsck.findBroken True g
	u <- liftAnnex getUUID
	void $ repairWhenNecessary urlrenderer u Nothing fsckresults
	mapM_ reget =<< liftAnnex (dirKeys gitAnnexBadDir)
  where
	reget k = queueTransfers "fsck found bad file; redownloading" Next k Nothing Download
runActivity' urlrenderer (ScheduledRemoteFsck u s d) = dispatch =<< liftAnnex (remoteFromUUID u)
  where
	dispatch Nothing = debug ["skipping remote fsck of uuid without a configured remote", fromUUID u, fromSchedule s]
	dispatch (Just rmt) = void $ case Remote.remoteFsck rmt of
		Nothing -> go rmt $ do
			program <- programPath
			void $ batchCommand program $ 
				[ Param "fsck"
				-- avoid downloading files
				, Param "--fast"
				, Param "--from"
				, Param $ Remote.name rmt
				] ++ annexFsckParams d
		Just mkfscker -> do
			{- Note that having mkfsker return an IO action
			 - avoids running a long duration fsck in the
			 - Annex monad. -}
			go rmt =<< liftAnnex (mkfscker (annexFsckParams d))
	go rmt annexfscker = do
		fsckresults <- showFscking urlrenderer (Just rmt) $ tryNonAsync $ do
			void annexfscker
			let r = Remote.repo rmt
			if Git.repoIsLocal r && not (Git.repoIsLocalUnknown r)
				then Just <$> Git.Fsck.findBroken True r
				else pure Nothing
		maybe noop (void . repairWhenNecessary urlrenderer u (Just rmt)) fsckresults

annexFsckParams :: Duration -> [CommandParam]
annexFsckParams d =
	[ Param "--incremental-schedule=1d"
	, Param $ "--time-limit=" ++ fromDuration d
	]
