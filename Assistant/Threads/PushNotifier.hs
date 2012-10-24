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
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Pushes
import qualified Remote

import Network.Protocol.XMPP
import Network
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S
import Utility.FileMode

thisThread :: ThreadName
thisThread = "PushNotifier"

pushNotifierThread :: ThreadState -> DaemonStatusHandle -> PushNotifier -> NamedThread
pushNotifierThread st dstatus pushnotifier = NamedThread thisThread $ do
	v <- runThreadState st $ getXMPPCreds
	case v of
		Nothing -> nocreds
		Just c -> case parseJID (xmppUsername c) of
			Nothing -> nocreds
			Just jid -> void $ client c jid
	where
		nocreds = do
			-- TODO alert
			return () -- exit thread

		client c jid = runClient server jid (xmppUsername c) (xmppPassword c) $ do
			void $ bindJID jid
			void $ putStanza $ emptyPresence PresenceUnavailable
			s <- getSession
			_ <- liftIO $ forkIO $ void $ sendnotifications s
			receivenotifications
			where
				server = Server
					(JID Nothing (jidDomain jid) Nothing)
					(xmppHostname c)
					(PortNumber $ fromIntegral $ xmppPort c)

		sendnotifications session = runXMPP session $ forever $ do
			us <- liftIO $ waitPush pushnotifier
			{- Toggle presence to send the notification. -}
			putStanza $ (emptyPresence PresenceAvailable)
				{ presenceID = Just $ encodePushNotification us }
			putStanza $ emptyPresence PresenceUnavailable

		receivenotifications = forever $ do
			s <- getStanza
			case s of
				ReceivedPresence (Presence { presenceType = PresenceAvailable, presenceID = Just t }) ->
					 maybe noop (liftIO . pull dstatus)
						(decodePushNotification t)
				_ -> noop

{- Everything we need to know to connect to an XMPP server. -}
data XMPPCreds = XMPPCreds
	{ xmppUsername :: T.Text
	, xmppPassword :: T.Text
	, xmppHostname :: HostName
	, xmppPort :: Int
	}
	deriving (Read, Show)

getXMPPCreds :: Annex (Maybe XMPPCreds)
getXMPPCreds = do
	f <- xmppCredsFile
	s <- liftIO $ catchMaybeIO $ readFile f
	return $ readish =<< s

setXMPPCreds :: XMPPCreds -> Annex ()
setXMPPCreds creds = do
	f <- xmppCredsFile
	liftIO $ do
		h <- openFile f WriteMode
		modifyFileMode f $ removeModes
			[groupReadMode, otherReadMode]
		hPutStr h (show creds)
		hClose h	

xmppCredsFile :: Annex FilePath
xmppCredsFile = do
	dir <- fromRepo gitAnnexCredsDir
	return $ dir </> "notify-xmpp"

{- A push notification is encoded in the id field of an XMPP presence
 - notification, in the form: "git-annex-push:uuid[:uuid:...]
 - 
 - Git repos can be pushed to that do not have a git-annex uuid; an empty
 - string is used for those.
 -}
prefix :: T.Text
prefix = T.pack "git-annex-push:"

delim :: T.Text
delim = T.pack ":"

encodePushNotification :: [UUID] -> T.Text
encodePushNotification us = T.concat 
	[ prefix
	, T.intercalate delim $ map (T.pack . fromUUID) us
	]

decodePushNotification :: T.Text -> Maybe [UUID]
decodePushNotification t = map (toUUID . T.unpack) . T.splitOn delim
	<$> T.stripPrefix prefix t

pull :: DaemonStatusHandle -> [UUID] -> IO ()
pull _ [] = noop
pull dstatus us = do
	rs <- filter matching . syncRemotes <$> getDaemonStatus dstatus
	print ("TODO pull from", rs)
	where
		matching r = Remote.uuid r `S.member` s
		s = S.fromList us
