{- git-annex XMPP client
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.XMPPClient where

import Assistant.Common
import Assistant.XMPP
import Assistant.XMPP.Client
import Assistant.NetMessager
import Assistant.Types.NetMessager
import Assistant.Types.Buddies
import Assistant.XMPP.Buddies
import Assistant.Sync
import Assistant.DaemonStatus
import qualified Remote
import Utility.ThreadScheduler
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.Alert
import Assistant.Pairing

import Network.Protocol.XMPP
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Git.Branch
import Data.Time.Clock

xmppClientThread :: UrlRenderer -> NamedThread
xmppClientThread urlrenderer = NamedThread "XMPPClient" $ do
	{- All Assistant actions have to be converted into IO actions that
	 - can be run from within the XMPP monad using liftIO. Ugly. -}
	iodebug <- asIO1 debug
	iopull <- asIO1 pull
	iopairReqReceived <- asIO2 $ pairReqReceived urlrenderer
	ioupdatebuddies <- asIO1 $ \p ->
		updateBuddyList (updateBuddies p) <<~ buddyList
	ioemptybuddies <- asIO $
		updateBuddyList (const noBuddies) <<~ buddyList
	iorelay <- asIO1 relayNetMessage
	ioclientthread <- asIO $
		go iorelay iodebug iopull ioupdatebuddies ioemptybuddies iopairReqReceived
	restartableClient ioclientthread
  where
	go iorelay iodebug iopull ioupdatebuddies ioemptybuddies iopairReqReceived = do
		v <- liftAnnex getXMPPCreds
		case v of
			Nothing -> noop
			Just c -> liftIO $ loop c =<< getCurrentTime
	  where
		debug' = void . liftIO . iodebug

		{- When the client exits, it's restarted;
 		 - if it keeps failing, back off to wait 5 minutes before
 		 - trying it again. -}
		loop c starttime = do
			runclient c
			now <- getCurrentTime
			if diffUTCTime now starttime > 300
				then do
					void $ iodebug ["connection lost; reconnecting"]
					loop c now
				else do
					void $ iodebug ["connection failed; will retry"]
					threadDelaySeconds (Seconds 300)
					loop c =<< getCurrentTime

		runclient c = void $ connectXMPP c $ \jid -> do
			fulljid <- bindJID jid
			debug' ["connected", show fulljid]
			{- The buddy list starts empty each time
			 - the client connects, so that stale info
			 - is not retained. -}
			void $ liftIO ioemptybuddies
			putStanza $ gitAnnexPresence gitAnnexSignature
			xmppThread $ receivenotifications fulljid
			forever $ do
				a <- liftIO $ iorelay fulljid
				a

		receivenotifications fulljid = forever $ do
			l <- decodeStanza fulljid <$> getStanza
			debug' ["received:", show l]
			mapM_ handle l

		handle (PresenceMessage p) =
			void $ liftIO $ ioupdatebuddies p
		handle (GotNetMessage QueryPresence) =
			putStanza $ gitAnnexPresence gitAnnexSignature
		handle (GotNetMessage (NotifyPush us)) =
			void $ liftIO $ iopull us
		handle (GotNetMessage (PairingNotification stage t u)) =
			maybe noop (handlePairing stage u) (parseJID t)
		handle (Ignorable _) = noop
		handle (Unknown _) = noop
		handle (ProtocolError _) = noop

		handlePairing PairReq u jid = liftIO $ iopairReqReceived u jid
		handlePairing PairAck _ _ = error "TODO"
		handlePairing PairDone _ _ = error "TODO"

data XMPPEvent
	= GotNetMessage NetMessage
	| PresenceMessage Presence
	| Ignorable Presence
	| Unknown ReceivedStanza
	| ProtocolError ReceivedStanza
	deriving Show

{- Decodes an XMPP stanza into one or more events. -}
decodeStanza :: JID -> ReceivedStanza -> [XMPPEvent]
decodeStanza fulljid s@(ReceivedPresence p)
	| presenceType p == PresenceError = [ProtocolError s]
	| presenceFrom p == Nothing = [Ignorable p]
	| presenceFrom p == Just fulljid = [Ignorable p]
	| not (null pushed) = impliedp $ GotNetMessage $ NotifyPush pushed
	| isPresenceQuery p = impliedp $ GotNetMessage QueryPresence
	| otherwise = case decodePairingNotification p of
		Nothing -> [PresenceMessage p]
		Just pn -> impliedp $ GotNetMessage pn
  where
	-- Things sent via presence imply a presence message,
	-- along with their real meaning.
	impliedp v = [PresenceMessage p, v]
	pushed = concat $ catMaybes $ map decodePushNotification $
		presencePayloads p
decodeStanza _ s = [Unknown s]

{- Waits for a NetMessager message to be sent, and relays it to XMPP. -}
relayNetMessage :: JID -> Assistant (XMPP ())
relayNetMessage fulljid = convert <$> waitNetMessage
  where
	convert (NotifyPush us) = putStanza $ pushNotification us
	convert QueryPresence = putStanza $ presenceQuery
	convert (PairingNotification stage t u) = case parseJID t of
		Nothing -> noop
		Just tojid -> mapM_ putStanza $
			pairingNotification stage u tojid fulljid

{- Runs the client, handing restart events. -}
restartableClient :: IO () -> Assistant ()
restartableClient a = forever $ do
	tid <- liftIO $ forkIO a
	waitNetMessagerRestart
	liftIO $ killThread tid

{- Runs a XMPP action in a separate thread, using a session to allow it
 - to access the same XMPP client. -}
xmppThread :: XMPP () -> XMPP ()
xmppThread a = do
	s <- getSession
	void $ liftIO $ forkIO $
		void $ runXMPP s a

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
pull :: [UUID] -> Assistant ()
pull [] = noop
pull us = do
	rs <- filter matching . syncRemotes <$> getDaemonStatus
	debug $ "push notification for" : map (fromUUID . Remote.uuid ) rs
	pullone rs =<< liftAnnex (inRepo Git.Branch.current)
  where
	matching r = Remote.uuid r `S.member` s
	s = S.fromList us

	pullone [] _ = noop
	pullone (r:rs) branch =
		unlessM (all id . fst <$> manualPull branch [r]) $
			pullone rs branch

{- Show an alert when a PairReq is seen, unless the PairReq came from
 - another client using our JID. In that case, just start pairing. -}
pairReqReceived :: UrlRenderer -> UUID -> JID -> Assistant ()
pairReqReceived urlrenderer u jid = do
	-- TODO: check same JID
	let route = FinishXMPPPairR (PairKey u $ formatJID jid)
	url <- liftIO $ renderUrl urlrenderer route []
	close <- asIO1 removeAlert
	void $ addAlert $ pairRequestReceivedAlert (T.unpack $ buddyName jid)
		AlertButton
			{ buttonUrl = url
			, buttonLabel = T.pack "Respond"
			, buttonAction = Just close
			}
