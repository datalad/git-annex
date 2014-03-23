{- git-annex desktop notifications
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Notification where

import Common.Annex
import qualified Annex
import Logs.Transfer
#ifdef WITH_DBUS_NOTIFICATIONS
import Types.DesktopNotify
import qualified DBus.Notify as Notify
import qualified DBus.Client
#endif

-- Witness that notification has happened.
data NotifyWitness = NotifyWitness

{- Wrap around an action that performs a transfer, which may run multiple
 - attempts. Displays notification when supported and when the user asked
 - for it. -}
notifyTransfer :: Direction -> Maybe FilePath -> (NotifyWitness -> Annex Bool) -> Annex Bool
notifyTransfer _ Nothing a = a NotifyWitness
notifyTransfer direction (Just f) a = do
#ifdef WITH_DBUS_NOTIFICATIONS
	wanted <- Annex.getState Annex.desktopnotify
	let action = if direction == Upload then "uploading" else "downloading"
	let basedesc = action ++ " " ++ f
	let startdesc = "started " ++ basedesc
	let enddesc ok = if ok
		then "finished " ++ basedesc
		else basedesc ++ " failed"
	if (notifyStart wanted || notifyFinish wanted)
		then do
			client <- liftIO DBus.Client.connectSession
			startnotification <- liftIO $ if notifyStart wanted
				then Just <$> Notify.notify client (mkNote startdesc)
				else pure Nothing
			ok <- a NotifyWitness
			when (notifyFinish wanted) $ liftIO $ void $ maybe 
				(Notify.notify client $ mkNote $ enddesc ok)
				(\n -> Notify.replace client n $ mkNote $ enddesc ok)
				startnotification
			return ok
		else a NotifyWitness
#else
	a NotifyWitness
#endif

notifyDrop :: Maybe FilePath -> Bool -> Annex ()
notifyDrop Nothing _ = noop
notifyDrop (Just f) ok = do
#ifdef WITH_DBUS_NOTIFICATIONS
	wanted <- Annex.getState Annex.desktopnotify
	when (notifyFinish wanted) $ liftIO $ do
		client <- DBus.Client.connectSession
		let msg = if ok
			then "dropped " ++ f
			else "failed to drop" ++ f
		void $ Notify.notify client (mkNote msg)
#else
	noop
#endif

#ifdef WITH_DBUS_NOTIFICATIONS
mkNote :: String -> Notify.Note
mkNote desc = Notify.blankNote
	{ Notify.appName = "git-annex"
	, Notify.body = Just $ Notify.Text desc
	, Notify.hints =
		[ Notify.Category Notify.Transfer
		, Notify.Urgency Notify.Low
		, Notify.SuppressSound True
		]
	}
#endif
