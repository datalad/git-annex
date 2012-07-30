{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.DaemonStatus where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.Alert
import Utility.ThreadScheduler
import Utility.TempFile
import Utility.NotificationBroadcaster
import Logs.Transfer
import qualified Command.Sync

import Control.Concurrent.STM
import System.Posix.Types
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Map as M
import Control.Exception

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
	-- Messages to display to the user.
	, alertMap :: AlertMap
	, alertMax :: AlertId
	-- Ordered list of remotes to talk to.
	, knownRemotes :: [Remote]
	-- Broadcasts notifications about all changes to the DaemonStatus
	, changeNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when queued or current transfers change.
	, transferNotifier :: NotificationBroadcaster
	-- Broadcasts notifications when there's a change to the alerts
	, alertNotifier :: NotificationBroadcaster
	}

type TransferMap = M.Map Transfer TransferInfo

{- This TMVar is never left empty, so accessing it will never block. -}
type DaemonStatusHandle = TMVar DaemonStatus

newDaemonStatus :: IO DaemonStatus
newDaemonStatus = DaemonStatus
	<$> pure False
	<*> pure Nothing
	<*> pure False
	<*> pure Nothing
	<*> pure M.empty
	<*> pure M.empty
	<*> pure 0
	<*> pure []
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster
	<*> newNotificationBroadcaster

getDaemonStatus :: DaemonStatusHandle -> IO DaemonStatus
getDaemonStatus = atomically . readTMVar

modifyDaemonStatus_ :: DaemonStatusHandle -> (DaemonStatus -> DaemonStatus) -> IO ()
modifyDaemonStatus_ dstatus a = modifyDaemonStatus dstatus $ \s -> (a s, ())

modifyDaemonStatus :: DaemonStatusHandle -> (DaemonStatus -> (DaemonStatus, b)) -> IO b
modifyDaemonStatus dstatus a = do
	(s, b) <- atomically $ do
		r@(s, _) <- a <$> takeTMVar dstatus
		putTMVar dstatus s
		return r
	sendNotification $ changeNotifier s
	return b

{- Updates the cached ordered list of remotes from the list in Annex
 - state. -}
updateKnownRemotes :: DaemonStatusHandle -> Annex ()
updateKnownRemotes dstatus = do
	remotes <- Command.Sync.syncRemotes []
	liftIO $ modifyDaemonStatus_ dstatus $
		\s -> s { knownRemotes = remotes }

{- Load any previous daemon status file, and store it in a MVar for this
 - process to use as its DaemonStatus. Also gets current transfer status. -}
startDaemonStatus :: Annex DaemonStatusHandle
startDaemonStatus = do
	file <- fromRepo gitAnnexDaemonStatusFile
	status <- liftIO $
		catchDefaultIO (readDaemonStatusFile file) =<< newDaemonStatus
	transfers <- M.fromList <$> getTransfers
	remotes <- Command.Sync.syncRemotes []
	liftIO $ atomically $ newTMVar status
		{ scanComplete = False
		, sanityCheckRunning = False
		, currentTransfers = transfers
		, knownRemotes = remotes
		}

{- This writes the daemon status to disk, when it changes, but no more
 - frequently than once every ten minutes.
 -}
daemonStatusThread :: ThreadState -> DaemonStatusHandle -> IO ()
daemonStatusThread st dstatus = do
	notifier <- newNotificationHandle
		=<< changeNotifier <$> getDaemonStatus dstatus
	checkpoint
	runEvery (Seconds tenMinutes) $ do
		waitNotification notifier
		checkpoint
	where
		checkpoint = do
			status <- getDaemonStatus dstatus
			file <- runThreadState st $ fromRepo gitAnnexDaemonStatusFile
			writeDaemonStatusFile file status

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

{- Mutates the transfer map. Runs in STM so that the transfer map can
 - be modified in the same transaction that modifies the transfer queue.
 - Note that this does not send a notification of the change; that's left
 - to the caller. -}
adjustTransfersSTM :: DaemonStatusHandle -> (TransferMap -> TransferMap) -> STM ()
adjustTransfersSTM dstatus a = do
	s <- takeTMVar dstatus
	putTMVar dstatus $ s { currentTransfers = a (currentTransfers s) }

{- Variant that does send notifications. -}
adjustTransfers :: DaemonStatusHandle -> (TransferMap -> TransferMap) -> IO ()
adjustTransfers dstatus a = 
	notifyTransfer dstatus `after` modifyDaemonStatus_ dstatus go
	where
		go s = s { currentTransfers = a (currentTransfers s) }

{- Removes a transfer from the map, and returns its info. -}
removeTransfer :: DaemonStatusHandle -> Transfer -> IO (Maybe TransferInfo)
removeTransfer dstatus t = 
	notifyTransfer dstatus `after` modifyDaemonStatus dstatus go
	where
		go s =
			let (info, ts) = M.updateLookupWithKey
				(\_k _v -> Nothing)
				t (currentTransfers s)
			in (s { currentTransfers = ts }, info)

{- Send a notification when a transfer is changed. -}
notifyTransfer :: DaemonStatusHandle -> IO ()
notifyTransfer dstatus = sendNotification
	=<< transferNotifier <$> atomically (readTMVar dstatus)

{- Send a notification when alerts are changed. -}
notifyAlert :: DaemonStatusHandle -> IO ()
notifyAlert dstatus = sendNotification
	=<< alertNotifier <$> atomically (readTMVar dstatus)

{- Returns the alert's identifier, which can be used to remove it. -}
addAlert :: DaemonStatusHandle -> Alert -> IO AlertId
addAlert dstatus alert = notifyAlert dstatus `after` modifyDaemonStatus dstatus go
	where
		go s = (s { alertMax = i, alertMap = m }, i)
			where
				i = alertMax s + 1
				m = M.insertWith' const i alert (alertMap s)

removeAlert :: DaemonStatusHandle -> AlertId -> IO ()
removeAlert dstatus i = updateAlert dstatus i (const Nothing)

updateAlert :: DaemonStatusHandle -> AlertId -> (Alert -> Maybe Alert) -> IO ()
updateAlert dstatus i a = updateAlertMap dstatus $ \m -> M.update a i m

updateAlertMap :: DaemonStatusHandle -> (AlertMap -> AlertMap) -> IO ()
updateAlertMap dstatus a = notifyAlert dstatus `after` modifyDaemonStatus_ dstatus go
	where
		go s = s { alertMap = a (alertMap s) }

{- Displays an alert while performing an activity.
 -
 - The alert is left visible afterwards, as filler.
 - Old filler is pruned, to prevent the map growing too large. -}
alertWhile :: DaemonStatusHandle -> Alert -> IO Bool -> IO Bool
alertWhile dstatus alert a = do
	let alert' = alert { alertClass = Activity }
	i <- addAlert dstatus alert'
	r <- bracket_ noop noop a
	updateAlertMap dstatus $ convertToFiller i r
	return r
