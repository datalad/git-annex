{- git-annex desktop notifications
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
notifyTransfer :: Transferrable t => Observable v => Direction -> t -> (NotifyWitness -> Annex v) -> Annex v
#ifdef WITH_DBUS_NOTIFICATIONS
notifyTransfer direction t a = case descTransfrerrable t of
	Nothing -> a NotifyWitness
	Just desc -> do
		wanted <- Annex.getState Annex.desktopnotify
		if (notifyStart wanted || notifyFinish wanted)
			then do
				client <- liftIO DBus.Client.connectSession
				startnotification <- liftIO $ if notifyStart wanted
					then Just <$> Notify.notify client (startedTransferNote direction desc)
					else pure Nothing
				res <- a NotifyWitness
				let ok = observeBool res
				when (notifyFinish wanted) $ liftIO $ void $ maybe 
					(Notify.notify client $ finishedTransferNote ok direction desc)
					(\n -> Notify.replace client n $ finishedTransferNote ok direction desc)
					startnotification
				return res
			else a NotifyWitness
#else
notifyTransfer _ _ a = a NotifyWitness
#endif

notifyDrop :: AssociatedFile -> Bool -> Annex ()
notifyDrop (AssociatedFile Nothing) _ = noop
#ifdef WITH_DBUS_NOTIFICATIONS
notifyDrop (AssociatedFile (Just f)) ok = do
	wanted <- Annex.getState Annex.desktopnotify
	when (notifyFinish wanted) $ liftIO $ do
		client <- DBus.Client.connectSession
		void $ Notify.notify client (droppedNote ok (fromRawFilePath f))
#else
notifyDrop (AssociatedFile (Just _)) _ = noop
#endif

#ifdef WITH_DBUS_NOTIFICATIONS
startedTransferNote :: Direction -> String -> Notify.Note
startedTransferNote Upload   = mkNote Notify.Transfer Notify.Low iconUpload
	"Uploading"
startedTransferNote Download = mkNote Notify.Transfer Notify.Low iconDownload
	"Downloading"

finishedTransferNote :: Bool -> Direction -> String -> Notify.Note
finishedTransferNote False Upload   = mkNote Notify.TransferError Notify.Normal iconFailure
	"Failed to upload"
finishedTransferNote False Download = mkNote Notify.TransferError Notify.Normal iconFailure
	"Failed to download"
finishedTransferNote True  Upload   = mkNote Notify.TransferComplete Notify.Low iconSuccess
	"Finished uploading"
finishedTransferNote True  Download = mkNote Notify.TransferComplete Notify.Low iconSuccess
	"Finished downloading"

droppedNote :: Bool -> String -> Notify.Note
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
