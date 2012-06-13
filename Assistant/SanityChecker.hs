{- git-annex assistant sanity checker
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.SanityChecker (
	sanityCheckerThread
) where

import Common.Annex
import Assistant.DaemonStatus
import Assistant.ThreadedMonad
import Utility.ThreadScheduler

import Data.Time.Clock.POSIX

{- This thread wakes up occasionally to make sure the tree is in good shape. -}
sanityCheckerThread :: ThreadState -> DaemonStatusHandle -> IO ()
sanityCheckerThread st status = forever $ do
	waitForNextCheck st status

	runThreadState st $
		modifyDaemonStatus status $ \s -> s
				{ sanityCheckRunning = True }

	now <- getPOSIXTime -- before check started
	ok <- catchBoolIO $ runThreadState st check

	runThreadState st $ do
		modifyDaemonStatus status $ \s -> s
			{ sanityCheckRunning = False
			, lastSanityCheck =
				if ok
					then Just now
					else lastSanityCheck s
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
			| lastcheck < now = oneDay - truncate (now - lastcheck)
			| otherwise = oneDay

check  :: Annex Bool
check = do
	return True

oneDay :: Int
oneDay = 24 * 60 * 60
