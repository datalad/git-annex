{- git-annex assistant webapp utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.WebApp.Utility where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.Types.TransferSlots
import Assistant.TransferSlots
import Assistant.Sync
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.List as Remote
import qualified Assistant.Threads.Transferrer as Transferrer
import Logs.Transfer
import Locations.UserConfig
import qualified Config

import qualified Data.Map as M
import Control.Concurrent
import System.Posix.Signals (signalProcessGroup, sigTERM, sigKILL)
import System.Posix.Process (getProcessGroupIDOf)

{- Use Nothing to change global sync setting. -}
changeSyncable :: (Maybe Remote) -> Bool -> Handler ()
changeSyncable Nothing _ = noop -- TODO
changeSyncable (Just r) True = do
	changeSyncFlag r True
	syncRemote r
changeSyncable (Just r) False = do
	changeSyncFlag r False
	liftAssistant $ updateSyncRemotes
	{- Stop all transfers to or from this remote.
	 - XXX Can't stop any ongoing scan, or git syncs. -}
	void $ liftAssistant $ dequeueTransfers tofrom
	mapM_ (cancelTransfer False) =<<
		filter tofrom . M.keys <$>
			liftAssistant (currentTransfers <$> getDaemonStatus)
  where
	tofrom t = transferUUID t == Remote.uuid r

changeSyncFlag :: Remote -> Bool -> Handler ()
changeSyncFlag r enabled = runAnnex undefined $ do
	Config.setConfig key value
	void $ Remote.remoteListRefresh
  where
	key = Config.remoteConfig (Remote.repo r) "sync"
	value
		| enabled = "true"
		| otherwise = "false"

{- Start syncing remote, using a background thread. -}
syncRemote :: Remote -> Handler ()
syncRemote = liftAssistant . syncNewRemote

pauseTransfer :: Transfer -> Handler ()
pauseTransfer = cancelTransfer True

cancelTransfer :: Bool -> Transfer -> Handler ()
cancelTransfer pause t = do
	m <- getCurrentTransfers
	unless pause $
		{- remove queued transfer -}
		void $ liftAssistant $ dequeueTransfers $ equivilantTransfer t
	{- stop running transfer -}
	maybe noop stop (M.lookup t m)
  where
	stop info = liftAssistant $ do
		{- When there's a thread associated with the
		 - transfer, it's signaled first, to avoid it
		 - displaying any alert about the transfer having
		 - failed when the transfer process is killed. -}
		liftIO $ maybe noop signalthread $ transferTid info
		liftIO $ maybe noop killproc $ transferPid info
		if pause
			then void $ alterTransferInfo t $
				\i -> i { transferPaused = True }
			else void $ removeTransfer t
	signalthread tid
		| pause = throwTo tid PauseTransfer
		| otherwise = killThread tid
	{- In order to stop helper processes like rsync,
	 - kill the whole process group of the process running the transfer. -}
	killproc pid = do
		g <- getProcessGroupIDOf pid
		void $ tryIO $ signalProcessGroup sigTERM g
		threadDelay 50000 -- 0.05 second grace period
		void $ tryIO $ signalProcessGroup sigKILL g

startTransfer :: Transfer -> Handler ()
startTransfer t = do
	m <- getCurrentTransfers
	maybe startqueued go (M.lookup t m)
  where
	go info = maybe (start info) resume $ transferTid info
	startqueued = do
		is <- liftAssistant $ map snd <$> getMatchingTransfers (== t)
		maybe noop start $ headMaybe is
	resume tid = do
		liftAssistant $ alterTransferInfo t $
			\i -> i { transferPaused = False }
		liftIO $ throwTo tid ResumeTransfer
	start info = liftAssistant $ do
		program <- liftIO readProgramFile
		inImmediateTransferSlot $
			Transferrer.startTransfer program t info

getCurrentTransfers :: Handler TransferMap
getCurrentTransfers = currentTransfers <$> liftAssistant getDaemonStatus
