{- xmpp support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP where

import Assistant.Common
import Utility.FileMode
import Utility.SRV

import Network.Protocol.XMPP
import Network
import Control.Concurrent
import qualified Data.Text as T
import Data.XML.Types
import Control.Exception (SomeException)

{- Everything we need to know to connect to an XMPP server. -}
data XMPPCreds = XMPPCreds
	{ xmppUsername :: T.Text
	, xmppPassword :: T.Text
	, xmppHostname :: HostName
	, xmppPort :: Int
	, xmppJID :: T.Text
	}
	deriving (Read, Show)

connectXMPP :: XMPPCreds -> (JID -> XMPP a) -> IO (Either SomeException ())
connectXMPP c a = case parseJID (xmppJID c) of
	Nothing -> error "bad JID"
	Just jid -> connectXMPP' jid c a

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

	{- Async exceptions are let through so the XMPP thread can
	 - be killed. -}
	run h p a' = tryNonAsync $
		runClientError (Server serverjid h p) jid
			(xmppUsername c) (xmppPassword c) (void a')

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

{- A presence with a git-annex tag in it. -}
gitAnnexPresence :: Element -> Presence
gitAnnexPresence tag = (emptyPresence PresenceAvailable)
	{ presencePayloads = [extendedAway, tag] }
  where
	extendedAway = Element (Name (T.pack "show") Nothing Nothing) []
		[NodeContent $ ContentText $ T.pack "xa"]

{- Name of a git-annex tag, in our own XML namespace.
 - (Not using a namespace URL to avoid unnecessary bloat.) -}
gitAnnexTagName :: Name
gitAnnexTagName  = Name (T.pack "git-annex") (Just $ T.pack "git-annex") Nothing

{- A git-annex tag, to let other clients know we're a git-annex client too. -}
gitAnnexSignature :: Element
gitAnnexSignature = Element gitAnnexTagName [] []

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
