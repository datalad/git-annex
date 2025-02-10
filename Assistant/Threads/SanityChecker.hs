{- git-annex assistant sanity checker
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant.Threads.SanityChecker (
	sanityCheckerStartupThread,
	sanityCheckerDailyThread,
	sanityCheckerHourlyThread
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.Repair
import Assistant.Drop
import Assistant.Ssh
import Assistant.TransferQueue
import Assistant.Types.UrlRenderer
import Assistant.Restart
import qualified Annex.Branch
import qualified Git
import qualified Git.LsFiles
import qualified Git.Command.Batch
import qualified Git.Config
import Utility.ThreadScheduler
import qualified Assistant.Threads.Watcher as Watcher
import Utility.Batch
import Utility.NotificationBroadcaster
import Utility.HumanTime
import Utility.Tense
import Git.Repair
import Git.Index
import Assistant.Unused
import Logs.Unused
import Types.Transfer
import Annex.Path
import Annex.Tmp
import qualified Annex
import qualified Utility.RawFilePath as R
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
#endif
#ifndef mingw32_HOST_OS
import Utility.LogFile
import Utility.DiskFree
#endif

import Data.Time.Clock.POSIX
import qualified Data.Text as T
import System.PosixCompat.Files (statusChangeTime, isSymbolicLink)

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
			debug ["corrupt index file found at startup; removing and restaging"]
			liftAnnex $ inRepo $ removeWhenExistsWith removeFile . indexFile
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
		debug ["corrupt annex/index file found at startup; removing"]
		liftAnnex $ liftIO . removeWhenExistsWith removeFile =<< fromRepo gitAnnexIndex

	{- Fix up ssh remotes set up by past versions of the assistant. -}
	liftIO $ fixUpSshRemotes

	{- Clean up old temp files. -}
	void $ liftAnnex $ tryNonAsync $ cleanupOtherTmp

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
		liftAnnex $ warning $ UnquotedString $ show e
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
	checkRepoExists

	g <- liftAnnex gitRepo
	batchmaker <- liftIO getBatchCommandMaker

	-- Find old unstaged symlinks, and add them to git.
	(unstaged, cleanup) <- liftIO $ Git.LsFiles.notInRepo [] False [literalOsPath "."] g
	now <- liftIO getPOSIXTime
	forM_ unstaged $ \file -> do
		ms <- liftIO $ catchMaybeIO $ R.getSymbolicLinkStatus $ fromOsPath file
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
		liftIO $ void $ Git.Command.Batch.run batchmaker
			[ Param "-c", Param "gc.auto=670000"
			, Param "gc"
			, Param "--auto"
			] g

	{- Check if the unused files found last time have been dealt with. -}
	checkOldUnused urlrenderer

	{- Run git-annex unused once per day. This is run as a separate
	 - process to stay out of the annex monad and so it can run as a
	 - batch job. -}
	program <- fromOsPath <$> liftIO programPath
	let (program', params') = batchmaker (program, [Param "unused"])
	void $ liftIO $ boolSystem program' params'
	{- Invalidate unused keys cache, and queue transfers of all unused
	 - keys, or if no transfers are called for, drop them. -}
	unused <- liftAnnex unusedKeys'
	void $ liftAnnex $ setUnusedKeys unused
	forM_ unused $ \k -> do
		unlessM (queueTransfers "unused" Later k (AssociatedFile Nothing) Upload) $
			handleDrops "unused" True k (AssociatedFile Nothing) []

	return True
  where
	toonew timestamp now = now < (realToFrac (timestamp + slop) :: POSIXTime)
	slop = fromIntegral tenMinutes
	insanity msg = do
		liftAnnex $ warning (UnquotedString msg)
		void $ addAlert $ sanityCheckFixAlert msg
	addsymlink file s = do
		Watcher.runHandler Watcher.onAddSymlink file s
		insanity $ "found unstaged symlink: " ++ fromOsPath file

hourlyCheck :: Assistant ()
hourlyCheck = do
	checkRepoExists
#ifndef mingw32_HOST_OS
	checkLogSize 0
#else
	noop
#endif

#ifndef mingw32_HOST_OS
{- Rotate logs once when total log file size is > 2 mb.
 -
 - If total log size is larger than the amount of free disk space,
 - continue rotating logs until size is < 2 mb, even if this
 - results in immediately losing the just logged data.
 -}
checkLogSize :: Int -> Assistant ()
checkLogSize n = do
	f <- liftAnnex $ fromRepo gitAnnexDaemonLogFile
	logs <- liftIO $ listLogs (fromOsPath f)
	totalsize <- liftIO $ sum <$> mapM (getFileSize . toOsPath) logs
	when (totalsize > 2 * oneMegabyte) $ do
		debug ["Rotated logs due to size:", show totalsize]
		liftIO $ openLog (fromOsPath f) >>= handleToFd >>= redirLog
		when (n < maxLogs + 1) $ do
			df <- liftIO $ getDiskFree $ fromOsPath $ takeDirectory f
			case df of
				Just free
					| free < fromIntegral totalsize ->
						checkLogSize (n + 1)
				_ -> noop
  where
	oneMegabyte :: Integer
	oneMegabyte = 1000000
#endif

oneHour :: Int
oneHour = 60 * 60

oneDay :: Int
oneDay = 24 * oneHour

{- If annex.expireunused is set, find any keys that have lingered unused
 - for the specified duration, and remove them.
 -
 - Otherwise, check to see if unused keys are piling up, and let the user
 - know. -}
checkOldUnused :: UrlRenderer -> Assistant ()
checkOldUnused urlrenderer = go =<< annexExpireUnused <$> liftAnnex Annex.getGitConfig
  where
	go (Just Nothing) = noop
	go (Just (Just expireunused)) = expireUnused (Just expireunused)
	go Nothing = maybe noop promptconfig =<< describeUnusedWhenBig

	promptconfig msg = 
#ifdef WITH_WEBAPP
		do
			button <- mkAlertButton True (T.pack "Configure") urlrenderer ConfigUnusedR
			void $ addAlert $ unusedFilesAlert [button] $ T.unpack $ renderTense Present msg
#else
		debug [show $ renderTense Past msg]
#endif

checkRepoExists :: Assistant ()
checkRepoExists = do
	g <- liftAnnex gitRepo
	liftIO $ unlessM (doesDirectoryExist $ Git.repoPath g) $
		terminateSelf
