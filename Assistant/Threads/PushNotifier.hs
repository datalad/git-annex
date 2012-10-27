{- git-annex assistant push notification thread, using XMPP
 -
 - This handles both sending outgoing push notifications, and receiving
 - incoming push notifications.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.PushNotifier where

import Assistant.Common
import Assistant.XMPP
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Pushes
import Assistant.Sync
import qualified Remote
import Utility.ThreadScheduler

import Network.Protocol.XMPP
import Control.Concurrent
import qualified Data.Set as S
import qualified Git.Branch
import Data.Time.Clock

thisThread :: ThreadName
thisThread = "PushNotifier"

pushNotifierThread :: ThreadState -> DaemonStatusHandle -> PushNotifier -> NamedThread
pushNotifierThread st dstatus pushnotifier = NamedThread thisThread $ do
	v <- runThreadState st $ getXMPPCreds
	case v of
		Nothing -> return () -- no creds? exit thread
		Just c -> loop c =<< getCurrentTime
	where
		loop c starttime = do
			void $ connectXMPP c $ \jid -> do
				fulljid <- bindJID jid
				liftIO $ debug thisThread ["XMPP connected", show fulljid]
				s <- getSession
				_ <- liftIO $ forkIO $ void $ runXMPP s $
					receivenotifications
				sendnotifications
			now <- getCurrentTime
			if diffUTCTime now starttime > 300
				then do
					debug thisThread ["XMPP connection lost; reconnecting"]
					loop c now
				else do
					debug thisThread ["XMPP connection failed; will retry"]
					threadDelaySeconds (Seconds 300)
					loop c =<< getCurrentTime

		
		sendnotifications = forever $ do
			us <- liftIO $ waitPush pushnotifier
			let payload = [extendedAway, encodePushNotification us]
			let notification = (emptyPresence PresenceAvailable)
				{ presencePayloads = payload }
			putStanza notification

		receivenotifications = forever $ do
			s <- getStanza
			liftIO $ debug thisThread ["received XMPP:", show s]
			case s of
				ReceivedPresence p@(Presence { presenceType = PresenceAvailable }) ->
					liftIO $ pull st dstatus $
						concat $ catMaybes $
							map decodePushNotification $
								presencePayloads p
				_ -> noop

{- We only pull from one remote out of the set listed in the push
 - notification, as an optimisation.
 -
 - Note that it might be possible (though very unlikely) for the push
 - notification to take a while to be sent, and multiple pushes happen
 - before it is sent, so it includes multiple remotes that were pushed
 - to at different times. 
 -
 - It could then be the case that the remote we choose had the earlier
 - push sent to it, but then failed to get the later push, and so is not
 - fully up-to-date. If that happens, the pushRetryThread will come along
 - and retry the push, and we'll get another notification once it succeeds,
 - and pull again. -}
pull :: ThreadState -> DaemonStatusHandle -> [UUID] -> IO ()
pull _ _ [] = noop
pull st dstatus us = do
	rs <- filter matching . syncRemotes <$> getDaemonStatus dstatus
	debug thisThread $ "push notification for" :
		map (fromUUID . Remote.uuid ) rs
	pullone rs =<< runThreadState st (inRepo Git.Branch.current)
	where
		matching r = Remote.uuid r `S.member` s
		s = S.fromList us

		pullone [] _ = noop
		pullone (r:rs) branch =
			unlessM (all id . fst <$> manualPull st branch [r]) $
				pullone rs branch
