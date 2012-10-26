{- git-annex assistant XMPP configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.XMPP where

import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod
#ifdef WITH_XMPP
import Assistant.Common
import Assistant.XMPP
import Utility.SRV
#endif

import Yesod
#ifdef WITH_XMPP
import Network
import Network.Protocol.XMPP
import Data.Text (Text)
import qualified Data.Text as T
#endif

getXMPPR :: Handler RepHtml
#ifdef WITH_XMPP
getXMPPR = xmppPage $ do
	((result, form), enctype) <- lift $ do
		oldcreds <- runAnnex Nothing getXMPPCreds
		runFormGet $ renderBootstrap $ xmppAForm $
			creds2Form <$> oldcreds
	let showform problem = do
		let authtoken = webAppFormAuthToken
		$(widgetFile "configurators/xmpp")
	case result of
		FormSuccess f -> maybe (showform True) (lift . storecreds)
			=<< liftIO (validateForm f)
		_ -> showform False
	where
		storecreds creds = do
			void $ runAnnex undefined $ setXMPPCreds creds
			redirect ConfigR
#else
getXMPPR = xmppPage $
	$(widgetFile "configurators/xmpp/disabled")
#endif

#ifdef WITH_XMPP

data XMPPForm = XMPPForm
	{ formJID :: Text
	, formPassword :: Text }

creds2Form :: XMPPCreds -> XMPPForm
creds2Form c = XMPPForm (xmppJID c) (xmppPassword c)

xmppAForm :: (Maybe XMPPForm) -> AForm WebApp WebApp XMPPForm
xmppAForm def = XMPPForm
	<$> areq jidField "Jabber address" (formJID <$> def)
	<*> areq passwordField "Password" Nothing

jidField :: Field WebApp WebApp Text
jidField = checkBool (isJust . parseJID) bad textField
	where
		bad :: Text
		bad = "This should look like an email address.."

validateForm :: XMPPForm -> IO (Maybe XMPPCreds)
validateForm f = do
	let jid = fromMaybe (error "bad JID") $ parseJID (formJID f)
	let domain = T.unpack $ strDomain $ jidDomain jid
	hostports <- lookupSRV $ mkSRVTcp "xmpp-client" domain
	let username = fromMaybe "" (strNode <$> jidNode jid)
	case hostports of
		((h, PortNumber p):_) -> testXMPP $ XMPPCreds
			{ xmppUsername = username
			, xmppPassword = formPassword f
			, xmppHostname = h
			, xmppPort = fromIntegral p
			, xmppJID = formJID f
			}
		_ -> testXMPP $ XMPPCreds
			{ xmppUsername = username
			, xmppPassword = formPassword f
			, xmppHostname = T.unpack $ strDomain $ jidDomain jid
			, xmppPort = 5222
			, xmppJID = formJID f
			}

testXMPP :: XMPPCreds -> IO (Maybe XMPPCreds)
testXMPP creds = either (const $ return Nothing)
	(const $ return $ Just creds)
	=<< connectXMPP creds (const noop)

#endif

xmppPage :: Widget -> Handler RepHtml
xmppPage w = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Jabber"
	w
