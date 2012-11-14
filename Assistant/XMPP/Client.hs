{- xmpp client support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Client where

import Assistant.Common
import Utility.SRV
import Creds

import Network.Protocol.XMPP
import Network
import Control.Concurrent
import qualified Data.Text as T
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
getXMPPCreds = parse <$> readCacheCreds xmppCredsFile
  where
	parse s = readish =<< s

setXMPPCreds :: XMPPCreds -> Annex ()
setXMPPCreds creds = writeCacheCreds (show creds) xmppCredsFile

xmppCredsFile :: FilePath
xmppCredsFile = "xmpp"
