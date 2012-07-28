{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.DaemonStatus where

import Common.Annex
import Assistant.ThreadedMonad
import Utility.ThreadScheduler
import Utility.TempFile
import Utility.NotificationBroadcaster
import Logs.Transfer
import qualified Command.Sync

import Control.Concurrent
import System.Posix.Types
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Map as M

data DaemonStatus = DaemonStatus
	-- False when the daemon is performing its startup scan
	{ scanComplete :: Bool
	-- Time when a previous process of the daemon was running ok
	, lastRunning :: Maybe POSIXTime
	-- True when the sanity checker is running
	, sanityCheckRunning :: Bool
	-- Last time the sanity checker ran
	, lastSanityCheck :: Maybe POSIXTime
	-- Currently running file content transfers
	, currentTransfers :: TransferMap
	-- Ordered list of remotes to talk to.
	, knownRemotes :: [Remote]
	-- Clients can use this to wait on changes to the DaemonStatus
	, notificationBroadcaster :: NotificationBroadcaster
	}

type TransferMap = M.Map Transfer TransferInfo

type DaemonStatusHandle = MVar DaemonStatus

newDaemonStatus :: IO DaemonStatus
newDaemonStatus = do
	nb <- newNotificationBroadcaster
	return $ DaemonStatus
		{ scanComplete = False
		, lastRunning = Nothing
		, sanityCheckRunning = False
		, lastSanityCheck = Nothing
		, currentTransfers = M.empty
		, knownRemotes = []
		, notificationBroadcaster = nb
		}

getDaemonStatus :: DaemonStatusHandle -> Annex DaemonStatus
getDaemonStatus = liftIO . readMVar

modifyDaemonStatus_ :: DaemonStatusHandle -> (DaemonStatus -> DaemonStatus) -> Annex ()
modifyDaemonStatus_ handle a = do
	nb <- liftIO $ modifyMVar handle $ \s -> return
		(a s, notificationBroadcaster s)
	liftIO $ sendNotification nb

modifyDaemonStatus :: DaemonStatusHandle -> (DaemonStatus -> (DaemonStatus, b)) -> Annex b
modifyDaemonStatus handle a = do
	(b, nb) <- liftIO $ modifyMVar handle $ \s -> do
		let (s', b) = a s
		return $ (s', (b, notificationBroadcaster s))
	liftIO $ sendNotification nb
	return b

{- Updates the cached ordered list of remotes from the list in Annex
 - state. -}
updateKnownRemotes :: DaemonStatusHandle -> Annex ()
updateKnownRemotes dstatus = do
	remotes <- Command.Sync.syncRemotes []
	modifyDaemonStatus_ dstatus $
		\s -> s { knownRemotes = remotes }

{- Load any previous daemon status file, and store it in the MVar for this
 - process to use as its DaemonStatus. Also gets current transfer status. -}
startDaemonStatus :: Annex DaemonStatusHandle
startDaemonStatus = do
	file <- fromRepo gitAnnexDaemonStatusFile
	status <- liftIO $
		catchDefaultIO (readDaemonStatusFile file) =<< newDaemonStatus
	transfers <- M.fromList <$> getTransfers
	remotes <- Command.Sync.syncRemotes []
	liftIO $ newMVar status
		{ scanComplete = False
		, sanityCheckRunning = False
		, currentTransfers = transfers
		, knownRemotes = remotes
		}

{- This writes the daemon status to disk, when it changes, but no more
 - frequently than once every ten minutes.
 -}
daemonStatusThread :: ThreadState -> DaemonStatusHandle -> IO ()
daemonStatusThread st handle = do
	bhandle <- runThreadState st $ 
		liftIO . newNotificationHandle
			=<< notificationBroadcaster <$> getDaemonStatus handle
	checkpoint
	runEvery (Seconds tenMinutes) $ do
		liftIO $ waitNotification bhandle
		checkpoint
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
			, "lastSanityCheck:" ++ maybe "" show (lastSanityCheck status)
			]

readDaemonStatusFile :: FilePath -> IO DaemonStatus
readDaemonStatusFile file = parse <$> newDaemonStatus <*> readFile file
	where
		parse status = foldr parseline status . lines
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

{- Mutates the transfer map. -}
adjustTransfers :: DaemonStatusHandle -> (TransferMap -> TransferMap) -> Annex ()
adjustTransfers dstatus a = modifyDaemonStatus_ dstatus $
	\s -> s { currentTransfers = a (currentTransfers s) }

{- Removes a transfer from the map, and returns its info. -}
removeTransfer :: DaemonStatusHandle -> Transfer -> Annex (Maybe TransferInfo)
removeTransfer dstatus t = modifyDaemonStatus dstatus go
	where
		go s =
			let (info, ts) = M.updateLookupWithKey
				(\_k _v -> Nothing)
				t (currentTransfers s)
			in (s { currentTransfers = ts }, info)
