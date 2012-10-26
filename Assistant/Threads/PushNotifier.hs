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
import Utility.FileMode
import Utility.SRV

import Network.Protocol.XMPP
import Network
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Git.Branch
import Data.XML.Types
import Control.Exception as E

thisThread :: ThreadName
thisThread = "PushNotifier"

pushNotifierThread :: ThreadState -> DaemonStatusHandle -> PushNotifier -> NamedThread
pushNotifierThread st dstatus pushnotifier = NamedThread thisThread $ do
	v <- runThreadState st $ getXMPPCreds
	case v of
		Nothing -> return () -- no creds? exit thread
		Just c -> void $ connectXMPP c $ \jid -> do
			fulljid <- bindJID jid
			liftIO $ debug thisThread ["XMPP connected", show fulljid]
			s <- getSession
			_ <- liftIO $ forkOS $ void $ runXMPP s $
				receivenotifications
			sendnotifications
	where
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
	, xmppJID :: T.Text
	}
	deriving (Read, Show)

{- Note that this must be run in a bound thread; gnuTLS requires it. -}
connectXMPP :: XMPPCreds -> (JID -> XMPP a) -> IO (Either SomeException ())
connectXMPP c a = case parseJID (xmppJID c) of
	Nothing -> error "bad JID"
	Just jid -> runInBoundThread $ connectXMPP' jid c a

{- Do a SRV lookup, but if it fails, fall back to the cached xmppHostname. -}
connectXMPP' :: JID -> XMPPCreds -> (JID -> XMPP a) -> IO (Either SomeException ())
connectXMPP' jid c a = go =<< lookupSRV srvrecord
	where
		srvrecord = mkSRVTcp "xmpp-client" $
			T.unpack $ strDomain $ jidDomain jid
		serverjid = JID Nothing (jidDomain jid) Nothing

		go [] = run (xmppHostname c)
				(PortNumber $ fromIntegral $ xmppPort c)
				(a jid)
		go ((h,p):rest) = do
			{- Try each SRV record in turn, until one connects,
			 - at which point the MVar will be full. -}
			mv <- newEmptyMVar
			r <- run h p $ do
				liftIO $ putMVar mv ()
				a jid
			ifM (isEmptyMVar mv) (go rest, return r)

		run h p a' = do
			liftIO $ debug thisThread ["XMPP trying", h]
			E.try (runClientError (Server serverjid h p) jid (xmppUsername c) (xmppPassword c) (void a')) :: IO (Either SomeException ())

{- XMPP runClient, that throws errors rather than returning an Either -}
runClientError :: Server -> JID -> T.Text -> T.Text -> XMPP a -> IO a
runClientError s j u p x = either (error . show) return =<< runClient s j u p x

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
