{- git-annex desktop notifications
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Notification (NotifyWitness, noNotification, notifyTransfer, notifyDrop) where

import Annex.Common
import Types.Transfer
#ifdef WITH_DBUS_NOTIFICATIONS
import qualified Annex
import Types.DesktopNotify
import qualified DBus.Notify as Notify
import qualified DBus.Client
#endif

-- Witness that notification has happened.
data NotifyWitness = NotifyWitness

-- Only use when no notification should be done.
noNotification :: NotifyWitness
noNotification = NotifyWitness

{- Wrap around an action that performs a transfer, which may run multiple
 - attempts. Displays notification when supported and when the user asked
 - for it. -}
notifyTransfer :: Direction -> Maybe FilePath -> (NotifyWitness -> Annex Bool) -> Annex Bool
notifyTransfer _ Nothing a = a NotifyWitness
#ifdef WITH_DBUS_NOTIFICATIONS
notifyTransfer direction (Just f) a = do
	wanted <- Annex.getState Annex.desktopnotify
	if (notifyStart wanted || notifyFinish wanted)
		then do
			client <- liftIO DBus.Client.connectSession
			startnotification <- liftIO $ if notifyStart wanted
				then Just <$> Notify.notify client (startedTransferNote direction f)
				else pure Nothing
			ok <- a NotifyWitness
			when (notifyFinish wanted) $ liftIO $ void $ maybe 
				(Notify.notify client $ finishedTransferNote ok direction f)
				(\n -> Notify.replace client n $ finishedTransferNote ok direction f)
				startnotification
			return ok
		else a NotifyWitness
#else
notifyTransfer _ (Just _) a = a NotifyWitness
#endif

notifyDrop :: Maybe FilePath -> Bool -> Annex ()
notifyDrop Nothing _ = noop
#ifdef WITH_DBUS_NOTIFICATIONS
notifyDrop (Just f) ok = do
	wanted <- Annex.getState Annex.desktopnotify
	when (notifyFinish wanted) $ liftIO $ do
		client <- DBus.Client.connectSession
		void $ Notify.notify client (droppedNote ok f)
#else
notifyDrop (Just _) _ = noop
#endif

#ifdef WITH_DBUS_NOTIFICATIONS
startedTransferNote :: Direction -> FilePath -> Notify.Note
startedTransferNote Upload   = mkNote Notify.Transfer Notify.Low iconUpload
	"Uploading"
startedTransferNote Download = mkNote Notify.Transfer Notify.Low iconDownload
	"Downloading"

finishedTransferNote :: Bool -> Direction -> FilePath -> Notify.Note
finishedTransferNote False Upload   = mkNote Notify.TransferError Notify.Normal iconFailure
	"Failed to upload"
finishedTransferNote False Download = mkNote Notify.TransferError Notify.Normal iconFailure
	"Failed to download"
finishedTransferNote True  Upload   = mkNote Notify.TransferComplete Notify.Low iconSuccess
	"Finished uploading"
finishedTransferNote True  Download = mkNote Notify.TransferComplete Notify.Low iconSuccess
	"Finished downloading"

droppedNote :: Bool -> FilePath -> Notify.Note
droppedNote False = mkNote Notify.TransferError Notify.Normal iconFailure
	"Failed to drop"
droppedNote True  = mkNote Notify.TransferComplete Notify.Low iconSuccess
	"Dropped"

iconUpload, iconDownload, iconFailure, iconSuccess :: String
iconUpload   = "network-transmit"
iconDownload = "network-receive"
iconFailure  = "dialog-error"
iconSuccess  = "git-annex"  -- Is there a standard icon for success/completion?

mkNote :: Notify.Category -> Notify.UrgencyLevel -> String -> String -> FilePath -> Notify.Note
mkNote category urgency icon desc path = Notify.blankNote
	{ Notify.appName = "git-annex"
	, Notify.appImage = Just (Notify.Icon icon)
	, Notify.summary = desc ++ " " ++ path
	, Notify.hints =
		[ Notify.Category category
		, Notify.Urgency urgency
		, Notify.SuppressSound True
		]
	}
#endif
