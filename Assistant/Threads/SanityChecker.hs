{- git-annex assistant sanity checker
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.SanityChecker (
	sanityCheckerThread
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.ThreadedMonad
import Assistant.Changes
import Assistant.TransferQueue
import qualified Git.LsFiles
import Utility.ThreadScheduler
import qualified Assistant.Threads.Watcher as Watcher

import Data.Time.Clock.POSIX

thisThread :: ThreadName
thisThread = "SanityChecker"

{- This thread wakes up occasionally to make sure the tree is in good shape. -}
sanityCheckerThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> IO ()
sanityCheckerThread st status transferqueue changechan = forever $ do
	waitForNextCheck st status

	debug thisThread ["starting sanity check"]

	runThreadState st $
		modifyDaemonStatus_ status $ \s -> s
				{ sanityCheckRunning = True }

	now <- getPOSIXTime -- before check started
	catchIO (check st status transferqueue changechan)
		(runThreadState st . warning . show)

	runThreadState st $ do
		modifyDaemonStatus_ status $ \s -> s
			{ sanityCheckRunning = False
			, lastSanityCheck = Just now
			}
	
	debug thisThread ["sanity check complete"]


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
check :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> IO () 
check st status transferqueue changechan = do
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
			Watcher.runHandler thisThread st status
				transferqueue changechan
				Watcher.onAddSymlink file s
