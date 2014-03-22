{- git-annex transfers
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Transfer (
	module X,
	upload,
	download,
	runTransfer,
	notifyTransfer,	
	NotifyWitness,
	noRetry,
	forwardRetry,
) where

import qualified Annex
import Logs.Transfer as X
import Annex.Perms
import Annex.Exception
import Utility.Metered
#ifdef WITH_DBUS_NOTIFICATIONS
import Common.Annex
import Types.DesktopNotify
import qualified DBus.Notify as Notify
import qualified DBus.Client
#endif
#ifdef mingw32_HOST_OS
import Utility.WinLock
#endif

import Control.Concurrent

upload :: UUID -> Key -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex Bool) -> NotifyWitness -> Annex Bool
upload u key f d a _witness = runTransfer (Transfer Upload u key) f d a

download :: UUID -> Key -> AssociatedFile -> RetryDecider -> (MeterUpdate -> Annex Bool) -> NotifyWitness -> Annex Bool
download u key f d a _witness = runTransfer (Transfer Download u key) f d a

{- Runs a transfer action. Creates and locks the lock file while the
 - action is running, and stores info in the transfer information
 - file.
 -
 - If the transfer action returns False, the transfer info is 
 - left in the failedTransferDir.
 -
 - If the transfer is already in progress, returns False.
 -
 - An upload can be run from a read-only filesystem, and in this case
 - no transfer information or lock file is used.
 -}
runTransfer :: Transfer -> Maybe FilePath -> RetryDecider -> (MeterUpdate -> Annex Bool) -> Annex Bool
runTransfer t file shouldretry a = do
	info <- liftIO $ startTransferInfo file
	(meter, tfile, metervar) <- mkProgressUpdater t info
	mode <- annexFileMode
	(fd, inprogress) <- liftIO $ prep tfile mode info
	if inprogress
		then do
			showNote "transfer already in progress"
			return False
		else do
			ok <- retry info metervar $
		 		bracketIO (return fd) (cleanup tfile) (const $ a meter)
			unless ok $ recordFailedTransfer t info
			return ok
  where
#ifndef mingw32_HOST_OS
	prep tfile mode info = do
		mfd <- catchMaybeIO $
			openFd (transferLockFile tfile) ReadWrite (Just mode)
				defaultFileFlags { trunc = True }
		case mfd of
			Nothing -> return (Nothing, False)
			Just fd -> do
				locked <- catchMaybeIO $
					setLock fd (WriteLock, AbsoluteSeek, 0, 0)
				if isNothing locked
					then return (Nothing, True)
					else do
						void $ tryIO $ writeTransferInfoFile info tfile
						return (mfd, False)
#else
	prep tfile _mode info = do
		v <- catchMaybeIO $ lockExclusive (transferLockFile tfile)
		case v of
			Nothing -> return (Nothing, False)
			Just Nothing -> return (Nothing, True)
			Just (Just lockhandle) -> do
				void $ tryIO $ writeTransferInfoFile info tfile
				return (Just lockhandle, False)
#endif
	cleanup _ Nothing = noop
	cleanup tfile (Just lockhandle) = do
		void $ tryIO $ removeFile tfile
#ifndef mingw32_HOST_OS
		void $ tryIO $ removeFile $ transferLockFile tfile
		closeFd lockhandle
#else
		{- Windows cannot delete the lockfile until the lock
		 - is closed. So it's possible to race with another
		 - process that takes the lock before it's removed,
		 - so ignore failure to remove.
		 -}
		dropLock lockhandle
		void $ tryIO $ removeFile $ transferLockFile tfile
#endif
	retry oldinfo metervar run = do
		v <- tryAnnex run
		case v of
			Right b -> return b
			Left _ -> do
				b <- getbytescomplete metervar
				let newinfo = oldinfo { bytesComplete = Just b }
				if shouldretry oldinfo newinfo
					then retry newinfo metervar run
					else return False
	getbytescomplete metervar
		| transferDirection t == Upload =
			liftIO $ readMVar metervar
		| otherwise = do
			f <- fromRepo $ gitAnnexTmpObjectLocation (transferKey t)
			liftIO $ catchDefaultIO 0 $
				fromIntegral . fileSize <$> getFileStatus f

type RetryDecider = TransferInfo -> TransferInfo -> Bool

noRetry :: RetryDecider
noRetry _ _ = False

{- Retries a transfer when it fails, as long as the failed transfer managed
 - to send some data. -}
forwardRetry :: RetryDecider
forwardRetry old new = bytesComplete old < bytesComplete new

-- Witness that notification has happened.
data NotifyWitness = NotifyWitness

{- Wrap around an action that performs a transfer, which may run multiple
 - attempts, and displays notification when supported. -}
notifyTransfer :: Direction -> Maybe FilePath -> (NotifyWitness -> Annex Bool) -> Annex Bool
notifyTransfer _ Nothing a = a NotifyWitness
notifyTransfer direction (Just f) a = do
#ifdef WITH_DBUS_NOTIFICATIONS
	wanted <- Annex.getState Annex.desktopnotify
	let action = if direction == Upload then "uploading" else "downloading"
	let basedesc = action ++ " " ++ f
	let startdesc = "started " ++ basedesc
	let enddesc = "finished " ++ basedesc
	if (notifyStart wanted || notifyFinish wanted)
		then do
			client <- liftIO DBus.Client.connectSession
			let mknote desc = Notify.blankNote
				{ Notify.appName = "git-annex"
				, Notify.body = Just $ Notify.Text desc
				, Notify.hints =
					[ Notify.Category Notify.Transfer
					, Notify.Urgency Notify.Low
					, Notify.SuppressSound True
					]
				}
			startnotification <- liftIO $ if notifyStart wanted
				then Just <$> Notify.notify client (mknote startdesc)
				else pure Nothing
			r <- a NotifyWitness
			when (notifyFinish wanted) $ liftIO $ void $ maybe 
				(Notify.notify client $ mknote enddesc)
				(\n -> Notify.replace client n $ mknote enddesc)
				startnotification
			return r
		else a NotifyWitness
#else
	a NotifyWitness
#endif
