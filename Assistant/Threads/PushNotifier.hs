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
import Assistant.Sync
import qualified Remote

import Network.Protocol.XMPP
import Network
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S
import Utility.FileMode
import qualified Git.Branch
import Data.XML.Types

thisThread :: ThreadName
thisThread = "PushNotifier"

pushNotifierThread :: ThreadState -> DaemonStatusHandle -> PushNotifier -> NamedThread
pushNotifierThread st dstatus pushnotifier = NamedThread thisThread $ do
	v <- runThreadState st $ getXMPPCreds
	case v of
		Nothing -> nocreds
		Just c -> case parseJID (xmppJID c) of
			Nothing -> nocreds
			Just jid -> void $ client c jid
	where
		nocreds = do
			error "no creds" -- TODO alert
			return () -- exit thread

		client c jid = runClient server jid (xmppUsername c) (xmppPassword c) $ do
			void $ bindJID jid
			s <- getSession
			_ <- liftIO $ forkIO $ void $ runXMPP s $
				receivenotifications
			sendnotifications
			where
				server = Server
					(JID Nothing (jidDomain jid) Nothing)
					(xmppHostname c)
					(PortNumber $ fromIntegral $ xmppPort c)

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

{- Everything we need to know to connect to an XMPP server. -}
data XMPPCreds = XMPPCreds
	{ xmppUsername :: T.Text
	, xmppPassword :: T.Text
	, xmppHostname :: HostName
	, xmppPort :: Int
	{- Something like username@hostname, but not necessarily the same
	 - username or hostname used to connect to the server. -}
	, xmppJID :: T.Text
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

{- Marks the client as extended away. -}
extendedAway :: Element
extendedAway = Element (Name (T.pack "show") Nothing Nothing) []
	[NodeContent $ ContentText $ T.pack "xa"]

{- Name of a git-annex tag, in our own XML namespace.
 - (Not using a namespace URL to avoid unnecessary bloat.) -}
gitAnnexTagName :: Name
gitAnnexTagName  = Name (T.pack "git-annex") (Just $ T.pack "git-annex") Nothing

pushAttr :: Name
pushAttr = Name (T.pack "push") Nothing Nothing

uuidSep :: T.Text
uuidSep = T.pack ","

{- git-annex tag with one push attribute per UUID pushed to. -}
encodePushNotification :: [UUID] -> Element
encodePushNotification us = Element gitAnnexTagName
	[(pushAttr, [ContentText pushvalue])] []
	where
		pushvalue = T.intercalate uuidSep $
			map (T.pack . fromUUID) us

decodePushNotification :: Element -> Maybe [UUID]
decodePushNotification (Element name attrs _nodes)
	| name == gitAnnexTagName && not (null us) = Just us
	| otherwise = Nothing
	where
		us = map (toUUID . T.unpack) $
			concatMap (T.splitOn uuidSep . T.concat . map fromContent . snd) $
			filter ispush attrs
		ispush (k, _) = k == pushAttr
		fromContent (ContentText t) = t
		fromContent (ContentEntity t) = t

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
