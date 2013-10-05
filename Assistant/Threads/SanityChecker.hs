{- git-annex assistant sanity checker
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.SanityChecker (
	sanityCheckerStartupThread,
	sanityCheckerDailyThread,
	sanityCheckerHourlyThread
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Alert
import qualified Git.LsFiles
import qualified Git.Command
import qualified Git.Config
import Utility.ThreadScheduler
import qualified Assistant.Threads.Watcher as Watcher
import Utility.LogFile
import Utility.Batch
import Utility.NotificationBroadcaster
import Config
import qualified Git
import qualified Utility.Lsof as Lsof

import Data.Time.Clock.POSIX

{- This thread runs once at startup, and most other threads wait for it
 - to finish. (However, the webapp thread does not, to prevent the UI
 - being nonresponsive.) -}
sanityCheckerStartupThread :: NamedThread
sanityCheckerStartupThread = namedThreadUnchecked "SanityCheckerStartup" $
	startupCheck

{- This thread wakes up hourly for inxepensive frequent sanity checks. -}
sanityCheckerHourlyThread :: NamedThread
sanityCheckerHourlyThread = namedThread "SanityCheckerHourly" $ forever $ do
	liftIO $ threadDelaySeconds $ Seconds oneHour
	hourlyCheck

{- This thread wakes up daily to make sure the tree is in good shape. -}
sanityCheckerDailyThread :: NamedThread
sanityCheckerDailyThread = namedThread "SanityCheckerDaily" $ forever $ do
	waitForNextCheck

	debug ["starting sanity check"]
	void $ alertWhile sanityCheckAlert go
	debug ["sanity check complete"]
  where
	go = do
		modifyDaemonStatus_ $ \s -> s { sanityCheckRunning = True }

		now <- liftIO getPOSIXTime -- before check started
		r <- either showerr return =<< (tryIO . batch) <~> dailyCheck

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

startupCheck :: Assistant ()
startupCheck = do
	checkStaleGitLocks

	{- Notify other threads that the startup sanity check is done. -}
	status <- getDaemonStatus
	liftIO $ sendNotification $ startupSanityCheckNotifier status

{- It's important to stay out of the Annex monad as much as possible while
 - running potentially expensive parts of this check, since remaining in it
 - will block the watcher. -}
dailyCheck :: Assistant Bool
dailyCheck = do
	g <- liftAnnex gitRepo

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
		liftIO $ void $ Git.Command.runBool
			[ Param "-c", Param "gc.auto=670000"
			, Param "gc"
			, Param "--auto"
			] g

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
hourlyCheck = checkLogSize 0

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

{- Detect when a git lock file exists and has no git process currently
 - writing to it. This strongly suggests it is a stale lock file.
 -
 - However, this could be on a network filesystem. Which is not very safe
 - anyway (the assistant relies on being able to check when files have
 - no writers to know when to commit them). Just in case, when the lock
 - file appears stale, we delay for one minute, and check its size. If
 - the size changed, delay for another minute, and so on. This will at
 - least work to detect is another machine is writing out a new index
 - file, since git does so by writing the new content to index.lock.
 -}
checkStaleGitLocks :: Assistant ()
checkStaleGitLocks = do
	lockfiles <- filter (not . isInfixOf "gc.pid") 
		. filter (".lock" `isSuffixOf`)
		<$> (liftIO . dirContentsRecursiveSkipping (annexDir `isInfixOf`)
			=<< liftAnnex (fromRepo Git.localGitDir))
	checkStaleLocks lockfiles
checkStaleLocks :: [FilePath] -> Assistant ()
checkStaleLocks lockfiles = go =<< getsizes
  where
  	getsize lf = catchMaybeIO $ 
		(\s -> (lf, fileSize s)) <$> getFileStatus lf
  	getsizes = liftIO $ catMaybes <$> mapM getsize lockfiles
	go [] = return ()
	go l = ifM (liftIO $ null <$> Lsof.query ("--" : map fst l))
		( do
			waitforit "to check stale git lock file"
			l' <- getsizes
			if l' == l
				then liftIO $ mapM_ nukeFile (map fst l)
				else go l'
		, do
			waitforit "for git lock file writer"
			go =<< getsizes
		)
	waitforit why = do
		notice ["Waiting for 60 seconds", why]
		liftIO $ threadDelaySeconds $ Seconds 60

oneMegabyte :: Int
oneMegabyte = 1000000

oneHour :: Int
oneHour = 60 * 60

oneDay :: Int
oneDay = 24 * oneHour

