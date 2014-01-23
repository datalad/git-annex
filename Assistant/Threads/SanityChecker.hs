{- git-annex assistant sanity checker
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.SanityChecker (
	sanityCheckerStartupThread,
	sanityCheckerDailyThread,
	sanityCheckerHourlyThread
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.Repair
import Assistant.Ssh
import Assistant.TransferQueue
import Assistant.Types.UrlRenderer
import qualified Annex.Branch
import qualified Git
import qualified Git.LsFiles
import qualified Git.Command
import qualified Git.Config
import Utility.ThreadScheduler
import qualified Assistant.Threads.Watcher as Watcher
import Utility.LogFile
import Utility.Batch
import Utility.NotificationBroadcaster
import Utility.DiskFree
import Config
import Utility.HumanTime
import Utility.DataUnits
import Git.Repair
import Git.Index
import Logs.Unused
import Logs.Location
import Logs.Transfer
import Annex.Content
import Config.Files
import Types.Key
import qualified Annex
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
#endif

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import qualified Data.Text as T

{- This thread runs once at startup, and most other threads wait for it
 - to finish. (However, the webapp thread does not, to prevent the UI
 - being nonresponsive.) -}
sanityCheckerStartupThread :: Maybe Duration -> NamedThread
sanityCheckerStartupThread startupdelay = namedThreadUnchecked "SanityCheckerStartup" $ do
	{- Stale git locks can prevent commits from happening, etc. -}
	void $ repairStaleGitLocks =<< liftAnnex gitRepo

	{- A corrupt index file can prevent the assistant from working at
	 - all, so detect and repair. -}
	ifM (not <$> liftAnnex (inRepo checkIndexFast))
		( do
			notice ["corrupt index file found at startup; removing and restaging"]
			liftAnnex $ inRepo $ nukeFile . indexFile
			{- Normally the startup scan avoids re-staging files,
			 - but with the index deleted, everything needs to be
			 - restaged. -}
			modifyDaemonStatus_ $ \s -> s { forceRestage = True }
		, whenM (liftAnnex $ inRepo missingIndex) $ do
			debug ["no index file; restaging"]
			modifyDaemonStatus_ $ \s -> s { forceRestage = True }
		)
	{- If the git-annex index file is corrupt, it's ok to remove it;
	 - the data from the git-annex branch will be used, and the index
	 - will be automatically regenerated. -}
	unlessM (liftAnnex $ Annex.Branch.withIndex $ inRepo $ Git.Repair.checkIndexFast) $ do
		notice ["corrupt annex/index file found at startup; removing"]
		liftAnnex $ liftIO . nukeFile =<< fromRepo gitAnnexIndex

	{- Fix up ssh remotes set up by past versions of the assistant. -}
	liftIO $ fixUpSshRemotes

	{- If there's a startup delay, it's done here. -}
	liftIO $ maybe noop (threadDelaySeconds . Seconds . fromIntegral . durationSeconds) startupdelay

	{- Notify other threads that the startup sanity check is done. -}
	status <- getDaemonStatus
	liftIO $ sendNotification $ startupSanityCheckNotifier status

{- This thread wakes up hourly for inxepensive frequent sanity checks. -}
sanityCheckerHourlyThread :: NamedThread
sanityCheckerHourlyThread = namedThread "SanityCheckerHourly" $ forever $ do
	liftIO $ threadDelaySeconds $ Seconds oneHour
	hourlyCheck

{- This thread wakes up daily to make sure the tree is in good shape. -}
sanityCheckerDailyThread :: UrlRenderer -> NamedThread
sanityCheckerDailyThread urlrenderer = namedThread "SanityCheckerDaily" $ forever $ do
	waitForNextCheck

	debug ["starting sanity check"]
	void $ alertWhile sanityCheckAlert go
	debug ["sanity check complete"]
  where
	go = do
		modifyDaemonStatus_ $ \s -> s { sanityCheckRunning = True }

		now <- liftIO getPOSIXTime -- before check started
		r <- either showerr return 
			=<< (tryIO . batch) <~> dailyCheck urlrenderer

		modifyDaemonStatus_ $ \s -> s
			{ sanityCheckRunning = False
			, lastSanityCheck = Just now
			}

		return r

	showerr e = do
		liftAnnex $ warning $ show e
		return False

{- Only run one check per day, from the time of the last check. -}
waitForNextCheck :: Assistant ()
waitForNextCheck = do
	v <- lastSanityCheck <$> getDaemonStatus
	now <- liftIO getPOSIXTime
	liftIO $ threadDelaySeconds $ Seconds $ calcdelay now v
  where
	calcdelay _ Nothing = oneDay
	calcdelay now (Just lastcheck)
		| lastcheck < now = max oneDay $
			oneDay - truncate (now - lastcheck)
		| otherwise = oneDay

{- It's important to stay out of the Annex monad as much as possible while
 - running potentially expensive parts of this check, since remaining in it
 - will block the watcher. -}
dailyCheck :: UrlRenderer -> Assistant Bool
dailyCheck urlrenderer = do
	g <- liftAnnex gitRepo
	batchmaker <- liftIO getBatchCommandMaker

	-- Find old unstaged symlinks, and add them to git.
	(unstaged, cleanup) <- liftIO $ Git.LsFiles.notInRepo False ["."] g
	now <- liftIO getPOSIXTime
	forM_ unstaged $ \file -> do
		ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case ms of
			Just s	| toonew (statusChangeTime s) now -> noop
				| isSymbolicLink s -> addsymlink file ms
			_ -> noop
	liftIO $ void cleanup

	{- Allow git-gc to run once per day. More frequent gc is avoided
	 - by default to avoid slowing things down. Only run repacks when 100x
	 - the usual number of loose objects are present; we tend
	 - to have a lot of small objects and they should not be a
	 - significant size. -}
	when (Git.Config.getMaybe "gc.auto" g == Just "0") $
		liftIO $ void $ Git.Command.runBatch batchmaker
			[ Param "-c", Param "gc.auto=670000"
			, Param "gc"
			, Param "--auto"
			] g

	{- Check if the unused files found last time have been dealt with. -}
	checkOldUnused urlrenderer

	{- Run git-annex unused once per day. This is run as a separate
	 - process to stay out of the annex monad and so it can run as a
	 - batch job. -}
	program <- liftIO readProgramFile
	let (program', params') = batchmaker (program, [Param "unused"])
	void $ liftIO $ boolSystem program' params'
	{- Invalidate unused keys cache, and queue transfers of all unused
	 - keys. -}
	unused <- liftAnnex unusedKeys'
	void $ liftAnnex $ setUnusedKeys unused
	forM_ unused $ \k ->
		queueTransfers "unused" Later k Nothing Upload

	return True
  where
	toonew timestamp now = now < (realToFrac (timestamp + slop) :: POSIXTime)
	slop = fromIntegral tenMinutes
	insanity msg = do
		liftAnnex $ warning msg
		void $ addAlert $ sanityCheckFixAlert msg
	addsymlink file s = do
		isdirect <- liftAnnex isDirect
		Watcher.runHandler (Watcher.onAddSymlink isdirect) file s
		insanity $ "found unstaged symlink: " ++ file

hourlyCheck :: Assistant ()
hourlyCheck = do
	checkLogSize 0

{- Rotate logs until log file size is < 1 mb. -}
checkLogSize :: Int -> Assistant ()
checkLogSize n = do
	f <- liftAnnex $ fromRepo gitAnnexLogFile
	logs <- liftIO $ listLogs f
	totalsize <- liftIO $ sum <$> mapM filesize logs
	when (totalsize > oneMegabyte) $ do
		notice ["Rotated logs due to size:", show totalsize]
		liftIO $ openLog f >>= redirLog
		when (n < maxLogs + 1) $
			checkLogSize $ n + 1
  where
	filesize f = fromIntegral . fileSize <$> liftIO (getFileStatus f)

oneMegabyte :: Int
oneMegabyte = 1000000

oneHour :: Int
oneHour = 60 * 60

oneDay :: Int
oneDay = 24 * oneHour

{- If annex.expireunused is set, find any keys that have lingered unused
 - for the specified duration, and remove them.
 -
 - Otherwise, check to see if unused keys are piling up, and let the user
 - know. This uses heuristics: 1000 unused keys, or more unused keys 
 - than the remaining free disk space, or more than 1/10th the total
 - disk space being unused keys all suggest a problem. -}
checkOldUnused :: UrlRenderer -> Assistant ()
checkOldUnused urlrenderer = go =<< annexExpireUnused <$> liftAnnex Annex.getGitConfig
  where
  	go (Just expireunused) = do
		m <- liftAnnex $ readUnusedLog ""
		now <- liftIO getPOSIXTime
		let duration = durationToPOSIXTime expireunused
		let oldkeys = M.keys $ M.filter (tooold now duration) m
		forM_ oldkeys $ \k -> do
			debug ["removing old unused key", key2file k]
			liftAnnex $ do
				removeAnnex k
				logStatus k InfoMissing
	go Nothing = maybe noop prompt
		=<< toobig =<< liftAnnex (readUnusedLog "")

	tooold now duration (_, mt) =
		maybe False (\t -> now - t >= duration) mt

	toobig m = do
		let num = M.size m
		let diskused = foldl' sumkeysize 0 (M.keys m)
		df <- forpath getDiskFree
		disksize <- forpath getDiskSize
		return $ if moreused df diskused || tenthused disksize diskused
			then Just $ roughSize storageUnits False diskused ++
				" is used by old files"
			else if num > 1000
				then Just $ show num ++ " old files exist"
				else Nothing

	moreused Nothing _ = False
	moreused (Just df) used = df <= used

	tenthused Nothing _ = False
	tenthused (Just disksize) used = used >= disksize `div` 10

	sumkeysize s k = s + fromMaybe 0 (keySize k)

	forpath a = liftAnnex $ inRepo $ liftIO . a . Git.repoPath

	prompt msg = 
#ifdef WITH_WEBAPP
		do
			button <- mkAlertButton True (T.pack "Fix This") urlrenderer ConfigUnusedR
			void $ addAlert $ unusedFilesAlert [button] msg
#else
		debug [msg]
#endif
