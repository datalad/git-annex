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
import Assistant.XMPP.Git
import Annex.UUID

import Network.Protocol.XMPP
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Git.Branch
import Data.Time.Clock

xmppClientThread :: UrlRenderer -> NamedThread
xmppClientThread urlrenderer = NamedThread "XMPPClient" $ do
	{- All Assistant actions have to be converted into IO actions that
	 - can be run from within the XMPP monad using liftIO. Ugly. -}
	iodebug <- asIO1 debug
	iopull <- asIO1 pull
	iopairMsgReceived <- asIO2 $ pairMsgReceived urlrenderer
	ioupdatebuddies <- asIO1 $ \p ->
		updateBuddyList (updateBuddies p) <<~ buddyList
	ioemptybuddies <- asIO $
		updateBuddyList (const noBuddies) <<~ buddyList
	iorelay <- asIO1 relayNetMessage
	ioclientthread <- asIO $
		go iorelay iodebug iopull ioupdatebuddies ioemptybuddies iopairMsgReceived
	restartableClient ioclientthread
  where
	go iorelay iodebug iopull ioupdatebuddies ioemptybuddies iopairMsgReceived = do
		v <- liftAnnex getXMPPCreds
		case v of
			Nothing -> noop
			Just c -> liftIO $ retry (runclient c) =<< getCurrentTime
	  where
		debug' = void . liftIO . iodebug

		{- When the client exits, it's restarted;
 		 - if it keeps failing, back off to wait 5 minutes before
 		 - trying it again. -}
		retry a starttime = do
			e <- a
			now <- getCurrentTime
			if diffUTCTime now starttime > 300
				then do
					void $ iodebug ["connection lost; reconnecting", show e]
					retry a now
				else do
					void $ iodebug ["connection failed; will retry", show e]
					threadDelaySeconds (Seconds 300)
					retry a =<< getCurrentTime

		runclient c = void $ connectXMPP c $ \jid -> do
			selfjid <- bindJID jid
			debug' ["connected", show selfjid]
			{- The buddy list starts empty each time
			 - the client connects, so that stale info
			 - is not retained. -}
			void $ liftIO ioemptybuddies
			putStanza gitAnnexSignature
			xmppThread $ receivenotifications selfjid
			forever $ do
				a <- liftIO $ iorelay selfjid
				a

		receivenotifications selfjid = forever $ do
			l <- decodeStanza selfjid <$> getStanza
			debug' ["received:", show l]
			mapM_ (handle selfjid) l

		handle _ (PresenceMessage p) =
			void $ liftIO $ ioupdatebuddies p
		handle _ (GotNetMessage QueryPresence) =
			putStanza gitAnnexSignature
		handle _ (GotNetMessage (NotifyPush us)) =
			void $ liftIO $ iopull us
		handle selfjid (GotNetMessage (PairingNotification stage t u)) =
			maybe noop (\jid -> liftIO $ iopairMsgReceived (stage, u) (selfjid, jid)) (parseJID t)
		handle _ (Ignorable _) = noop
		handle _ (Unknown _) = noop
		handle _ (ProtocolError _) = noop

data XMPPEvent
	= GotNetMessage NetMessage
	| PresenceMessage Presence
	| Ignorable ReceivedStanza
	| Unknown ReceivedStanza
	| ProtocolError ReceivedStanza
	deriving Show

{- Decodes an XMPP stanza into one or more events. -}
decodeStanza :: JID -> ReceivedStanza -> [XMPPEvent]
decodeStanza selfjid s@(ReceivedPresence p)
	| presenceType p == PresenceError = [ProtocolError s]
	| presenceFrom p == Nothing = [Ignorable s]
	| presenceFrom p == Just selfjid = [Ignorable s]
	| otherwise = maybe [PresenceMessage p] decode (getGitAnnexAttrValue p)
  where
	decode (attr, v)
		| attr == pushAttr = impliedp $ GotNetMessage $ NotifyPush $
			decodePushNotification v
		| attr == queryAttr = impliedp $ GotNetMessage QueryPresence
		| otherwise = [Unknown s]
	{- Things sent via presence imply a presence message,
	 - along with their real meaning. -}
	impliedp v = [PresenceMessage p, v]
decodeStanza selfjid s@(ReceivedMessage m)
	| messageFrom m == Nothing = [Ignorable s]
	| messageFrom m == Just selfjid = [Ignorable s]
	| messageType m == MessageError = [ProtocolError s]
	| otherwise = maybe [Unknown s] decode (getGitAnnexAttrValue m)
  where
	decode (attr, v)
		| attr == pairAttr = 
			[maybe (Unknown s) GotNetMessage (decodePairingNotification v m)]
		| otherwise = [Unknown s]
decodeStanza _ s = [Unknown s]

{- Waits for a NetMessager message to be sent, and relays it to XMPP. -}
relayNetMessage :: JID -> Assistant (XMPP ())
relayNetMessage selfjid = convert =<< waitNetMessage
  where
	convert (NotifyPush us) = return $ putStanza $ pushNotification us
	convert QueryPresence = return $ putStanza $ presenceQuery
	convert (PairingNotification stage t u) = case parseJID t of
		Nothing -> return $ noop
		Just tojid
			| tojid == selfjid -> return $ noop
			| otherwise -> do
				changeBuddyPairing tojid True
				return $ putStanza $
					pairingNotification stage u tojid selfjid

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

pairMsgReceived :: UrlRenderer -> (PairStage, UUID) -> (JID, JID) -> Assistant ()
pairMsgReceived urlrenderer (PairReq, theiruuid) (selfjid, theirjid)
	-- PairReq from another client using our JID is automatically accepted.
	| baseJID selfjid == baseJID theirjid = do
		selfuuid <- liftAnnex getUUID
		sendNetMessage $
			PairingNotification PairAck (formatJID theirjid) selfuuid
		finishXMPPPairing theirjid theiruuid
	-- Show an alert to let the user decide if they want to pair.
	| otherwise = do
		let route = FinishXMPPPairR (PairKey theiruuid $ formatJID theirjid)
		url <- liftIO $ renderUrl urlrenderer route []
		close <- asIO1 removeAlert
		void $ addAlert $ pairRequestReceivedAlert (T.unpack $ buddyName theirjid)
			AlertButton
				{ buttonUrl = url
				, buttonLabel = T.pack "Respond"
				, buttonAction = Just close
				}
pairMsgReceived _ (PairAck, theiruuid) (_selfjid, theirjid) =
	{- PairAck must come from one of the buddies we are pairing with;
	 - don't pair with just anyone. -}
	whenM (isBuddyPairing theirjid) $ do
		changeBuddyPairing theirjid False
		selfuuid <- liftAnnex getUUID
		sendNetMessage $
			PairingNotification PairDone (formatJID theirjid) selfuuid
		finishXMPPPairing theirjid theiruuid
pairMsgReceived _ (PairDone, _theiruuid) (_selfjid, theirjid) =
	changeBuddyPairing theirjid False

isBuddyPairing :: JID -> Assistant Bool
isBuddyPairing jid = maybe False buddyPairing <$> 
	getBuddy (genBuddyKey jid) <<~ buddyList

changeBuddyPairing :: JID -> Bool -> Assistant ()
changeBuddyPairing jid ispairing =
	updateBuddyList (M.adjust set key) <<~ buddyList
  where
	key = genBuddyKey jid
	set b = b { buddyPairing = ispairing }
