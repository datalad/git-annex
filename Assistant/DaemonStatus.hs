{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.DaemonStatus where

import Common.Annex
import Assistant.ThreadedMonad
import Utility.ThreadScheduler
import Utility.TempFile

import Control.Concurrent
import System.Posix.Types
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale

data DaemonStatus = DaemonStatus
	-- False when the daemon is performing its startup scan
	{ scanComplete :: Bool
	-- Time when a previous process of the daemon was running ok
	, lastRunning :: Maybe POSIXTime
	-- True when the sanity checker is running
	, sanityCheckRunning :: Bool
	-- Last time the sanity checker ran
	, lastSanityCheck :: Maybe POSIXTime
	}
	deriving (Show)

type DaemonStatusHandle = MVar DaemonStatus

newDaemonStatus :: DaemonStatus
newDaemonStatus = DaemonStatus
	{ scanComplete = False
	, lastRunning = Nothing
	, sanityCheckRunning = False
	, lastSanityCheck = Nothing
	}

getDaemonStatus :: DaemonStatusHandle -> Annex DaemonStatus
getDaemonStatus = liftIO . readMVar

modifyDaemonStatus :: DaemonStatusHandle -> (DaemonStatus -> DaemonStatus) -> Annex ()
modifyDaemonStatus handle a = liftIO $ modifyMVar_ handle (return . a)

{- Load any previous daemon status file, and store it in the MVar for this
 - process to use as its DaemonStatus. -}
startDaemonStatus :: Annex DaemonStatusHandle
startDaemonStatus = do
	file <- fromRepo gitAnnexDaemonStatusFile
	status <- liftIO $
		catchDefaultIO (readDaemonStatusFile file) newDaemonStatus
	liftIO $ newMVar status
		{ scanComplete = False
		, sanityCheckRunning = False
		}

{- This thread wakes up periodically and writes the daemon status to disk. -}
daemonStatusThread :: ThreadState -> DaemonStatusHandle -> IO ()
daemonStatusThread st handle = do
	checkpoint
	runEvery (Seconds tenMinutes) checkpoint
	where
		checkpoint = runThreadState st $ do
			file <- fromRepo gitAnnexDaemonStatusFile
			status <- getDaemonStatus handle
			liftIO $ writeDaemonStatusFile file status

{- Don't just dump out the structure, because it will change over time,
 - and parts of it are not relevant. -}
writeDaemonStatusFile :: FilePath -> DaemonStatus -> IO ()
writeDaemonStatusFile file status = 
	viaTmp writeFile file =<< serialized <$> getPOSIXTime
	where
		serialized now = unlines
			[ "lastRunning:" ++ show now
			, "scanComplete:" ++ show (scanComplete status)
			, "sanityCheckRunning:" ++ show (sanityCheckRunning status)
			, "lastSanityCheck:" ++ show (lastSanityCheck status)
			]

readDaemonStatusFile :: FilePath -> IO DaemonStatus
readDaemonStatusFile file = parse <$> readFile file
	where
		parse = foldr parseline newDaemonStatus . lines
		parseline line status
			| key == "lastRunning" = parseval readtime $ \v ->
				status { lastRunning = Just v }
			| key == "scanComplete" = parseval readish $ \v ->
				status { scanComplete = v }
			| key == "sanityCheckRunning" = parseval readish $ \v ->
				status { sanityCheckRunning = v }
			| key == "lastSanityCheck" = parseval readtime $ \v ->
				status { lastSanityCheck = Just v }
			| otherwise = status -- unparsable line
			where
				(key, value) = separate (== ':') line
				parseval parser a = maybe status a (parser value)
				readtime s = do
					d <- parseTime defaultTimeLocale "%s%Qs" s
					Just $ utcTimeToPOSIXSeconds d

{- Checks if a time stamp was made after the daemon was lastRunning.
 -
 - Some slop is built in; this really checks if the time stamp was made
 - at least ten minutes after the daemon was lastRunning. This is to
 - ensure the daemon shut down cleanly, and deal with minor clock skew.
 -
 - If the daemon has never ran before, this always returns False.
 -}
afterLastDaemonRun :: EpochTime -> DaemonStatus -> Bool
afterLastDaemonRun timestamp status = maybe False (< t) (lastRunning status)
	where
		t = realToFrac (timestamp + slop) :: POSIXTime
		slop = fromIntegral tenMinutes

tenMinutes :: Int
tenMinutes = 10 * 60
