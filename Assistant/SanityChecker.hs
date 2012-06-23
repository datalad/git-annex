{- git-annex assistant sanity checker
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.SanityChecker (
	sanityCheckerThread
) where

import Common.Annex
import qualified Git.LsFiles
import Assistant.DaemonStatus
import Assistant.ThreadedMonad
import Assistant.Changes
import Utility.ThreadScheduler
import qualified Assistant.Watcher

import Data.Time.Clock.POSIX

{- This thread wakes up occasionally to make sure the tree is in good shape. -}
sanityCheckerThread :: ThreadState -> DaemonStatusHandle -> ChangeChan -> IO ()
sanityCheckerThread st status changechan = forever $ do
	waitForNextCheck st status

	runThreadState st $
		modifyDaemonStatus status $ \s -> s
				{ sanityCheckRunning = True }

	now <- getPOSIXTime -- before check started
	catchIO (check st status changechan)
		(runThreadState st . warning . show)

	runThreadState st $ do
		modifyDaemonStatus status $ \s -> s
			{ sanityCheckRunning = False
			, lastSanityCheck = Just now
			}

{- Only run one check per day, from the time of the last check. -}
waitForNextCheck :: ThreadState -> DaemonStatusHandle -> IO ()
waitForNextCheck st status = do
	v <- runThreadState st $
		lastSanityCheck <$> getDaemonStatus status
	now <- getPOSIXTime
	threadDelaySeconds $ Seconds $ calcdelay now v
	where
		calcdelay _ Nothing = oneDay
		calcdelay now (Just lastcheck)
			| lastcheck < now = max oneDay $
				oneDay - truncate (now - lastcheck)
			| otherwise = oneDay

oneDay :: Int
oneDay = 24 * 60 * 60

{- It's important to stay out of the Annex monad as much as possible while
 - running potentially expensive parts of this check, since remaining in it
 - will block the watcher. -}
check :: ThreadState -> DaemonStatusHandle -> ChangeChan -> IO () 
check st status changechan = do
	g <- runThreadState st $ do
		showSideAction "Running daily check"
		fromRepo id
	-- Find old unstaged symlinks, and add them to git.
	unstaged <- Git.LsFiles.notInRepo False ["."] g
	now <- getPOSIXTime
	forM_ unstaged $ \file -> do
		ms <- catchMaybeIO $ getSymbolicLinkStatus file
		case ms of
			Just s	| toonew (statusChangeTime s) now -> noop
				| isSymbolicLink s ->
					addsymlink file ms
			_ -> noop
	where
		toonew timestamp now = now < (realToFrac (timestamp + slop) :: POSIXTime)
		slop = fromIntegral tenMinutes
		insanity m = runThreadState st $ warning m
		addsymlink file s = do
			insanity $ "found unstaged symlink: " ++ file
			Assistant.Watcher.runHandler st status changechan
				Assistant.Watcher.onAddSymlink file s
