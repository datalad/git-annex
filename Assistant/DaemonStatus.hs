{- git-annex assistant daemon status
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Assistant.DaemonStatus where

import Assistant.Common
import Assistant.Alert.Utility
import Utility.Tmp
import Assistant.Types.NetMessager
import Utility.NotificationBroadcaster
import Types.Transfer
import Logs.Transfer
import Logs.Trust
import Logs.TimeStamp
import qualified Remote
import qualified Types.Remote as Remote
import qualified Git

import Control.Concurrent.STM
import System.Posix.Types
import Data.Time.Clock.POSIX
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

getDaemonStatus :: Assistant DaemonStatus
getDaemonStatus = (atomically . readTMVar) <<~ daemonStatusHandle

modifyDaemonStatus_ :: (DaemonStatus -> DaemonStatus) -> Assistant ()
modifyDaemonStatus_ a = modifyDaemonStatus $ \s -> (a s, ())

modifyDaemonStatus :: (DaemonStatus -> (DaemonStatus, b)) -> Assistant b
modifyDaemonStatus a = do
	dstatus <- getAssistant daemonStatusHandle
	liftIO $ do
		(s, b) <- atomically $ do
			r@(!s, _) <- a <$> takeTMVar dstatus
			putTMVar dstatus s
			return r
		sendNotification $ changeNotifier s
		return b

{- Returns a function that updates the lists of syncable remotes
 - and other associated information. -}
calcSyncRemotes :: Annex (DaemonStatus -> DaemonStatus)
calcSyncRemotes = do
	rs <- filter (remoteAnnexSync . Remote.gitconfig) .
		concat . Remote.byCost <$> Remote.remoteList
	alive <- trustExclude DeadTrusted (map Remote.uuid rs)
	let good r = Remote.uuid r `elem` alive
	let syncable = filter good rs
	let syncdata = filter (not . remoteAnnexIgnore . Remote.gitconfig) $
		filter (\r -> Remote.uuid r /= NoUUID) $
		filter (not . Remote.isXMPPRemote) syncable

	return $ \dstatus -> dstatus
		{ syncRemotes = syncable
		, syncGitRemotes = filter Remote.gitSyncableRemote syncable
		, syncDataRemotes = syncdata
		, syncingToCloudRemote = any iscloud syncdata
		}
  where
	iscloud r = not (Remote.readonly r) && Remote.availability r == Remote.GloballyAvailable

{- Updates the syncRemotes list from the list of all remotes in Annex state. -}
updateSyncRemotes :: Assistant ()
updateSyncRemotes = do
	modifyDaemonStatus_ =<< liftAnnex calcSyncRemotes
	status <- getDaemonStatus
	liftIO $ sendNotification $ syncRemotesNotifier status

	when (syncingToCloudRemote status) $
		updateAlertMap $
			M.filter $ \alert ->
				alertName alert /= Just CloudRepoNeededAlert

changeCurrentlyConnected :: (S.Set UUID -> S.Set UUID) -> Assistant ()
changeCurrentlyConnected sm = do
	modifyDaemonStatus_ $ \ds -> ds
		{ currentlyConnectedRemotes = sm (currentlyConnectedRemotes ds)
		}
	v <- currentlyConnectedRemotes <$> getDaemonStatus
	debug [show v]
	liftIO . sendNotification =<< syncRemotesNotifier <$> getDaemonStatus

updateScheduleLog :: Assistant ()
updateScheduleLog =
	liftIO . sendNotification =<< scheduleLogNotifier <$> getDaemonStatus

{- Load any previous daemon status file, and store it in a MVar for this
 - process to use as its DaemonStatus. Also gets current transfer status. -}
startDaemonStatus :: Annex DaemonStatusHandle
startDaemonStatus = do
	file <- fromRepo gitAnnexDaemonStatusFile
	status <- liftIO $
		flip catchDefaultIO (readDaemonStatusFile file) =<< newDaemonStatus
	transfers <- M.fromList <$> getTransfers
	addsync <- calcSyncRemotes
	liftIO $ atomically $ newTMVar $ addsync $ status
		{ scanComplete = False
		, sanityCheckRunning = False
		, currentTransfers = transfers
		}

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
		| key == "lastRunning" = parseval parsePOSIXTime $ \v ->
			status { lastRunning = Just v }
		| key == "scanComplete" = parseval readish $ \v ->
			status { scanComplete = v }
		| key == "sanityCheckRunning" = parseval readish $ \v ->
			status { sanityCheckRunning = v }
		| key == "lastSanityCheck" = parseval parsePOSIXTime $ \v ->
			status { lastSanityCheck = Just v }
		| otherwise = status -- unparsable line
	  where
		(key, value) = separate (== ':') line
		parseval parser a = maybe status a (parser value)

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
	let !v = a (currentTransfers s)
	putTMVar dstatus $ s { currentTransfers = v }

{- Checks if a transfer is currently running. -}
checkRunningTransferSTM :: DaemonStatusHandle -> Transfer -> STM Bool
checkRunningTransferSTM dstatus t = M.member t . currentTransfers
	<$> readTMVar dstatus

{- Alters a transfer's info, if the transfer is in the map. -}
alterTransferInfo :: Transfer -> (TransferInfo -> TransferInfo) -> Assistant ()
alterTransferInfo t a = updateTransferInfo' $ M.adjust a t

{- Updates a transfer's info. Adds the transfer to the map if necessary,
 - or if already present, updates it while preserving the old transferTid,
 - transferPaused, and bytesComplete values, which are not written to disk. -}
updateTransferInfo :: Transfer -> TransferInfo -> Assistant ()
updateTransferInfo t info = updateTransferInfo' $ M.insertWith' merge t info
  where
	merge new old = new
		{ transferTid = maybe (transferTid new) Just (transferTid old)
		, transferPaused = transferPaused new || transferPaused old
		, bytesComplete = maybe (bytesComplete new) Just (bytesComplete old)
		}

updateTransferInfo' :: (TransferMap -> TransferMap) -> Assistant ()
updateTransferInfo' a = notifyTransfer `after` modifyDaemonStatus_ update
  where
	update s = s { currentTransfers = a (currentTransfers s) }

{- Removes a transfer from the map, and returns its info. -}
removeTransfer :: Transfer -> Assistant (Maybe TransferInfo)
removeTransfer t = notifyTransfer `after` modifyDaemonStatus remove
  where
	remove s =
		let (info, ts) = M.updateLookupWithKey
			(\_k _v -> Nothing)
			t (currentTransfers s)
		in (s { currentTransfers = ts }, info)

{- Send a notification when a transfer is changed. -}
notifyTransfer :: Assistant ()
notifyTransfer = do
	dstatus <- getAssistant daemonStatusHandle
	liftIO $ sendNotification
		=<< transferNotifier <$> atomically (readTMVar dstatus)

{- Send a notification when alerts are changed. -}
notifyAlert :: Assistant ()
notifyAlert = do
	dstatus <- getAssistant daemonStatusHandle
	liftIO $ sendNotification
		=<< alertNotifier <$> atomically (readTMVar dstatus)

{- Returns the alert's identifier, which can be used to remove it. -}
addAlert :: Alert -> Assistant AlertId
addAlert alert = do
	notice [showAlert alert]
	notifyAlert `after` modifyDaemonStatus add
  where
	add s = (s { lastAlertId = i, alertMap = m }, i)
	  where
		!i = nextAlertId $ lastAlertId s
		!m = mergeAlert i alert (alertMap s)

removeAlert :: AlertId -> Assistant ()
removeAlert i = updateAlert i (const Nothing)

updateAlert :: AlertId -> (Alert -> Maybe Alert) -> Assistant ()
updateAlert i a = updateAlertMap $ \m -> M.update a i m

updateAlertMap :: (AlertMap -> AlertMap) -> Assistant ()
updateAlertMap a = notifyAlert `after` modifyDaemonStatus_ update
  where
	update s = 
		let !m = a (alertMap s)
		in s { alertMap = m }

{- Displays an alert while performing an activity that returns True on
 - success.
 -
 - The alert is left visible afterwards, as filler.
 - Old filler is pruned, to prevent the map growing too large. -}
alertWhile :: Alert -> Assistant Bool -> Assistant Bool
alertWhile alert a = alertWhile' alert $ do
	r <- a
	return (r, r)

{- Like alertWhile, but allows the activity to return a value too. -}
alertWhile' :: Alert -> Assistant (Bool, a) -> Assistant a
alertWhile' alert a = do
	let alert' = alert { alertClass = Activity }
	i <- addAlert alert'
	(ok, r) <- a
	updateAlertMap $ mergeAlert i $ makeAlertFiller ok alert'
	return r

{- Displays an alert while performing an activity, then removes it. -}
alertDuring :: Alert -> Assistant a -> Assistant a
alertDuring alert a = do
	i <- addAlert $ alert { alertClass = Activity }
	removeAlert  i `after` a

getXMPPClientID :: Remote -> ClientID
getXMPPClientID r = T.pack $ drop (length "xmpp::") (Git.repoLocation (Remote.repo r))
