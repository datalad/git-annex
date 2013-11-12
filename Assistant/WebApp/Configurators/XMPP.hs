{- git-annex assistant XMPP configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.XMPP where

import Assistant.WebApp.Common
import Assistant.WebApp.Notifications
import Utility.NotificationBroadcaster
#ifdef WITH_XMPP
import qualified Remote
import Assistant.XMPP.Client
import Assistant.XMPP.Buddies
import Assistant.Types.Buddies
import Assistant.NetMessager
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.WebApp.RepoList
import Assistant.WebApp.Configurators
import Assistant.XMPP
#endif

#ifdef WITH_XMPP
import Network.Protocol.XMPP
import Network
import qualified Data.Text as T
#endif

{- Displays an alert suggesting to configure XMPP. -}
xmppNeeded :: Handler ()
#ifdef WITH_XMPP
xmppNeeded = whenM (isNothing <$> liftAnnex getXMPPCreds) $ do
	urlrender <- getUrlRender
	void $ liftAssistant $ do
		close <- asIO1 removeAlert
		addAlert $ xmppNeededAlert $ AlertButton
			{ buttonLabel = "Configure a Jabber account"
			, buttonUrl = urlrender XMPPConfigR
			, buttonAction = Just close
			}
#else
xmppNeeded = return ()
#endif

{- When appropriate, displays an alert suggesting to configure a cloud repo
 - to suppliment an XMPP remote. -}
checkCloudRepos :: UrlRenderer -> Remote -> Assistant ()
#ifdef WITH_XMPP
checkCloudRepos urlrenderer r =
	unlessM (syncingToCloudRemote <$> getDaemonStatus) $ do
		buddyname <- getBuddyName $ Remote.uuid r
		button <- mkAlertButton True "Add a cloud repository" urlrenderer $
			NeedCloudRepoR $ Remote.uuid r
		void $ addAlert $ cloudRepoNeededAlert buddyname button
#else
checkCloudRepos _ _ = noop
#endif

#ifdef WITH_XMPP
{- Returns the name of the friend corresponding to a
 - repository's UUID, but not if it's our name. -}
getBuddyName :: UUID -> Assistant (Maybe String)
getBuddyName u = go =<< getclientjid
  where
	go Nothing = return Nothing
	go (Just myjid) = (T.unpack . buddyName <$>)
		. headMaybe 
		. filter (\j -> baseJID j /= baseJID myjid)
		. map fst
		. filter (\(_, r) -> Remote.uuid r == u)
		<$> getXMPPRemotes
	getclientjid = maybe Nothing parseJID . xmppClientID
		<$> getDaemonStatus
#endif

getNeedCloudRepoR :: UUID -> Handler Html
#ifdef WITH_XMPP
getNeedCloudRepoR for = page "Cloud repository needed" (Just Configuration) $ do
	buddyname <- liftAssistant $ getBuddyName for
	$(widgetFile "configurators/xmpp/needcloudrepo")
#else
getNeedCloudRepoR _ = xmppPage $
	$(widgetFile "configurators/xmpp/disabled")
#endif

getXMPPConfigR :: Handler Html
getXMPPConfigR = postXMPPConfigR

postXMPPConfigR :: Handler Html
postXMPPConfigR = xmppform DashboardR

getXMPPConfigForPairFriendR :: Handler Html
getXMPPConfigForPairFriendR = postXMPPConfigForPairFriendR

postXMPPConfigForPairFriendR :: Handler Html
postXMPPConfigForPairFriendR = xmppform StartXMPPPairFriendR

getXMPPConfigForPairSelfR :: Handler Html
getXMPPConfigForPairSelfR = postXMPPConfigForPairSelfR

postXMPPConfigForPairSelfR :: Handler Html
postXMPPConfigForPairSelfR = xmppform StartXMPPPairSelfR

xmppform :: Route WebApp -> Handler Html
#ifdef WITH_XMPP
xmppform next = xmppPage $ do
	((result, form), enctype) <- liftH $ do
		oldcreds <- liftAnnex getXMPPCreds
		runFormPostNoToken $ renderBootstrap $ xmppAForm $
			creds2Form <$> oldcreds
	let showform problem = $(widgetFile "configurators/xmpp")
	case result of
		FormSuccess f -> either (showform . Just) (liftH . storecreds)
			=<< liftIO (validateForm f)
		_ -> showform Nothing
  where
	storecreds creds = do
		void $ liftAnnex $ setXMPPCreds creds
		liftAssistant notifyNetMessagerRestart
		redirect next
#else
xmppform _ = xmppPage $
	$(widgetFile "configurators/xmpp/disabled")
#endif

{- Called by client to get a list of buddies.
 -
 - Returns a div, which will be inserted into the calling page.
 -}
getBuddyListR :: NotificationId -> Handler Html
getBuddyListR nid = do
	waitNotifier getBuddyListBroadcaster nid

	p <- widgetToPageContent buddyListDisplay
	giveUrlRenderer $ [hamlet|^{pageBody p}|]

buddyListDisplay :: Widget
buddyListDisplay = do
	autoUpdate ident NotifierBuddyListR (10 :: Int) (10 :: Int)
#ifdef WITH_XMPP
	myjid <- liftAssistant $ xmppClientID <$> getDaemonStatus
	let isself (BuddyKey b) = Just b == myjid
	buddies <- liftAssistant $ do
		pairedwith <- map fst <$> getXMPPRemotes
		catMaybes . map (buddySummary pairedwith)
			<$> (getBuddyList <<~ buddyList)
	$(widgetFile "configurators/xmpp/buddylist")
#else
	noop
#endif
  where
	ident = "buddylist"

#ifdef WITH_XMPP

getXMPPRemotes :: Assistant [(JID, Remote)]
getXMPPRemotes = catMaybes . map pair . filter isXMPPRemote . syncGitRemotes
	<$> getDaemonStatus
  where
  	pair r = maybe Nothing (\jid -> Just (jid, r)) $
		parseJID $ getXMPPClientID r

data XMPPForm = XMPPForm
	{ formJID :: Text
	, formPassword :: Text }

creds2Form :: XMPPCreds -> XMPPForm
creds2Form c = XMPPForm (xmppJID c) (xmppPassword c)

xmppAForm :: (Maybe XMPPForm) -> MkAForm XMPPForm
xmppAForm def = XMPPForm
	<$> areq jidField "Jabber address" (formJID <$> def)
	<*> areq passwordField "Password" Nothing

jidField :: MkField Text
jidField = checkBool (isJust . parseJID) bad textField
  where
	bad :: Text
	bad = "This should look like an email address.."

validateForm :: XMPPForm -> IO (Either String XMPPCreds)
validateForm f = do
	let jid = fromMaybe (error "bad JID") $ parseJID (formJID f)
	let username = fromMaybe "" (strNode <$> jidNode jid)
	testXMPP $ XMPPCreds
		{ xmppUsername = username
		, xmppPassword = formPassword f
		, xmppHostname = T.unpack $ strDomain $ jidDomain jid
		, xmppPort = 5222
		, xmppJID = formJID f
		}

testXMPP :: XMPPCreds -> IO (Either String XMPPCreds)
testXMPP creds = do
	(good, bad) <- partition (either (const False) (const True) . snd) 
		<$> connectXMPP creds (const noop)
	case good of
		(((h, PortNumber p), _):_) -> return $ Right $ creds
			{ xmppHostname = h
			, xmppPort = fromIntegral p
			}
		(((h, _), _):_) -> return $ Right $ creds
			{ xmppHostname = h
			}
		_ -> return $ Left $ intercalate "; " $ map formatlog bad
  where
  	formatlog ((h, p), Left e) = "host " ++ h ++ ":" ++ showport p ++ " failed: " ++ show e
  	formatlog _ = ""

	showport (PortNumber n) = show n
	showport (Service s) = s
	showport (UnixSocket s) = s
#endif

xmppPage :: Widget -> Handler Html
xmppPage = page "Jabber" (Just Configuration)
