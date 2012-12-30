{- git-annex assistant webapp configurator for pairing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Pairing
import Assistant.WebApp.Common
import Assistant.Types.Buddies
#ifdef WITH_PAIRING
import Assistant.Pairing.Network
import Assistant.Pairing.MakeRemote
import Assistant.Ssh
import Assistant.Alert
import Assistant.DaemonStatus
import Utility.Verifiable
import Utility.Network
import Annex.UUID
#endif
#ifdef WITH_XMPP
import Assistant.XMPP
import Assistant.XMPP.Client
import Assistant.XMPP.Buddies
import Assistant.XMPP.Git
import Network.Protocol.XMPP
import Assistant.Types.NetMessager
import Assistant.NetMessager
import Assistant.WebApp.Configurators
import Assistant.WebApp.Configurators.XMPP
#endif
import Utility.UserInfo
import Git

#ifdef WITH_PAIRING
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Data.Char
import qualified Control.Exception as E
import Control.Concurrent
#endif
#ifdef WITH_XMPP
import qualified Data.Set as S
#endif

getStartXMPPPairR :: Handler RepHtml
#ifdef WITH_XMPP
getStartXMPPPairR = ifM (isJust <$> runAnnex Nothing getXMPPCreds)
	( do
		{- Ask buddies to send presence info, to get
		 - the buddy list populated. -}
		liftAssistant $ sendNetMessage QueryPresence
		pairPage $
			$(widgetFile "configurators/pairing/xmpp/prompt")
	, redirect XMPPForPairingR -- go get XMPP configured, then come back
	)
#else
getStartXMPPPairR = noXMPPPairing

noXMPPPairing :: Handler RepHtml
noXMPPPairing = noPairing "XMPP"
#endif

{- Does pairing with an XMPP buddy, or with other clients sharing an
 - XMPP account. -}
getRunningXMPPPairR :: BuddyKey -> Handler RepHtml
#ifdef WITH_XMPP
getRunningXMPPPairR bid = do
	buddy <- liftAssistant $ getBuddy bid <<~ buddyList
	go $ S.toList . buddyAssistants <$> buddy
  where
	go (Just (clients@((Client exemplar):_))) = do
		creds <- runAnnex Nothing getXMPPCreds
		let ourjid = fromJust $ parseJID =<< xmppJID <$> creds
		let samejid = baseJID ourjid == baseJID exemplar
		liftAssistant $ do
			u <- liftAnnex getUUID
			forM_ clients $ \(Client c) -> sendNetMessage $
				PairingNotification PairReq (formatJID c) u
		xmppPairEnd True $ if samejid then Nothing else Just exemplar
	-- A buddy could have logged out, or the XMPP client restarted,
	-- and there be no clients to message; handle unforseen by going back.
	go _ = redirect StartXMPPPairR
#else
getRunningXMPPPairR _ = noXMPPPairing
#endif

{- Starts local pairing. -}
getStartLocalPairR :: Handler RepHtml
#ifdef WITH_PAIRING
getStartLocalPairR = promptSecret Nothing $
	startLocalPairing PairReq noop pairingAlert Nothing
#else
getStartLocalPairR = noLocalPairing

noLocalPairing :: Handler RepHtml
noLocalPairing = noPairing "local"
#endif

{- Runs on the system that responds to a local pair request; sets up the ssh
 - authorized key first so that the originating host can immediately sync
 - with us. -}
getFinishLocalPairR :: PairMsg -> Handler RepHtml
#ifdef WITH_PAIRING
getFinishLocalPairR msg = promptSecret (Just msg) $ \_ secret -> do
	repodir <- lift $ repoPath <$> runAnnex undefined gitRepo
	liftIO $ setup repodir
	startLocalPairing PairAck (cleanup repodir) alert uuid "" secret
  where
	alert = pairRequestAcknowledgedAlert (pairRepo msg) . Just
	setup repodir = setupAuthorizedKeys msg repodir
	cleanup repodir = removeAuthorizedKeys False repodir $
		remoteSshPubKey $ pairMsgData msg
	uuid = Just $ pairUUID $ pairMsgData msg
#else
getFinishLocalPairR _ = noLocalPairing
#endif

getConfirmXMPPPairR :: PairKey -> Handler RepHtml
#ifdef WITH_XMPP
getConfirmXMPPPairR pairkey@(PairKey _ t) = case parseJID t of
	Nothing -> error "bad JID"
	Just theirjid -> pairPage $ do
		let name = buddyName theirjid
		$(widgetFile "configurators/pairing/xmpp/confirm")
#else
getConfirmXMPPPairR _ = noXMPPPairing
#endif

getFinishXMPPPairR :: PairKey -> Handler RepHtml
#ifdef WITH_XMPP
getFinishXMPPPairR (PairKey theiruuid t) = case parseJID t of
	Nothing -> error "bad JID"
	Just theirjid -> do
		liftAssistant $ do
			selfuuid <- liftAnnex getUUID
			sendNetMessage $
				PairingNotification PairAck (formatJID theirjid) selfuuid
			finishXMPPPairing theirjid theiruuid
		xmppPairEnd False $ Just theirjid
#else
getFinishXMPPPairR _ = noXMPPPairing
#endif

#ifdef WITH_XMPP
xmppPairEnd :: Bool -> Maybe JID -> Handler RepHtml
xmppPairEnd inprogress theirjid = pairPage $ do
	let friend = buddyName <$> theirjid
	let cloudrepolist = repoListDisplay $ RepoSelector
		{ onlyCloud = True
		, onlyConfigured = False
		, includeHere = False
		}
	$(widgetFile "configurators/pairing/xmpp/end")
#endif

getRunningLocalPairR :: SecretReminder -> Handler RepHtml
#ifdef WITH_PAIRING
getRunningLocalPairR s = pairPage $ do
	let secret = fromSecretReminder s
	$(widgetFile "configurators/pairing/local/inprogress")
#else
getRunningLocalPairR _ = noLocalPairing
#endif

#ifdef WITH_PAIRING

{- Starts local pairing, at either the PairReq (initiating host) or 
 - PairAck (responding host) stage.
 -
 - Displays an alert, and starts a thread sending the pairing message,
 - which will continue running until the other host responds, or until
 - canceled by the user. If canceled by the user, runs the oncancel action.
 -
 - Redirects to the pairing in progress page.
 -}
startLocalPairing :: PairStage -> IO () -> (AlertButton -> Alert) -> Maybe UUID -> Text -> Secret -> Widget
startLocalPairing stage oncancel alert muuid displaysecret secret = do
	urlrender <- lift getUrlRender
	reldir <- fromJust . relDir <$> lift getYesod

	sendrequests <- lift $ liftAssistant $ asIO2 $ mksendrequests urlrender
	{- Generating a ssh key pair can take a while, so do it in the
	 - background. -}
	thread <- lift $ liftAssistant $ asIO $ do
		keypair <- liftIO $ genSshKeyPair
		pairdata <- liftIO $ PairData
			<$> getHostname
			<*> myUserName
			<*> pure reldir
			<*> pure (sshPubKey keypair)
			<*> (maybe genUUID return muuid)
		let sender = multicastPairMsg Nothing secret pairdata
		let pip = PairingInProgress secret Nothing keypair pairdata stage
		startSending pip stage $ sendrequests sender
	void $ liftIO $ forkIO thread

	lift $ redirect $ RunningLocalPairR $ toSecretReminder displaysecret
  where
	{- Sends pairing messages until the thread is killed,
	 - and shows an activity alert while doing it.
	 -
	 - The cancel button returns the user to the HomeR. This is
	 - not ideal, but they have to be sent somewhere, and could
	 - have been on a page specific to the in-process pairing
	 - that just stopped, so can't go back there.
	 -}
	mksendrequests urlrender sender _stage = do
		tid <- liftIO myThreadId
		let selfdestruct = AlertButton
			{ buttonLabel = "Cancel"
			, buttonUrl = urlrender HomeR
			, buttonAction = Just $ const $ do
				oncancel
				killThread tid
			}
		alertDuring (alert selfdestruct) $ liftIO $ do
			_ <- E.try (sender stage) :: IO (Either E.SomeException ())
			return ()

data InputSecret = InputSecret { secretText :: Maybe Text }

{- If a PairMsg is passed in, ensures that the user enters a secret
 - that can validate it. -}
promptSecret :: Maybe PairMsg -> (Text -> Secret -> Widget) -> Handler RepHtml
promptSecret msg cont = pairPage $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $
			InputSecret <$> aopt textField "Secret phrase" Nothing
	case result of
		FormSuccess v -> do
			let rawsecret = fromMaybe "" $ secretText v
			let secret = toSecret rawsecret
			case msg of
				Nothing -> case secretProblem secret of
					Nothing -> cont rawsecret secret
					Just problem ->
						showform form enctype $ Just problem
				Just m ->
					if verify (fromPairMsg m) secret
						then cont rawsecret secret
						else showform form enctype $ Just
							"That's not the right secret phrase."
		_ -> showform form enctype Nothing
  where
	showform form enctype mproblem = do
		let start = isNothing msg
		let badphrase = isJust mproblem
		let problem = fromMaybe "" mproblem
		let (username, hostname) = maybe ("", "")
			(\(_, v, a) -> (T.pack $ remoteUserName v, T.pack $ fromMaybe (showAddr a) (remoteHostName v)))
			(verifiableVal . fromPairMsg <$> msg)
		u <- T.pack <$> liftIO myUserName
		let sameusername = username == u
		$(widgetFile "configurators/pairing/local/prompt")

{- This counts unicode characters as more than one character,
 - but that's ok; they *do* provide additional entropy. -}
secretProblem :: Secret -> Maybe Text
secretProblem s
	| B.null s = Just "The secret phrase cannot be left empty. (Remember that punctuation and white space is ignored.)"
	| B.length s < 7 = Just "Enter a longer secret phrase, at least 6 characters, but really, a phrase is best! This is not a password you'll need to enter every day."
	| s == toSecret sampleQuote = Just "Speaking of foolishness, don't paste in the example I gave. Enter a different phrase, please!"
	| otherwise = Nothing

toSecret :: Text -> Secret
toSecret s = B.fromChunks [T.encodeUtf8 $ T.toLower $ T.filter isAlphaNum s]

{- From Dickens -}
sampleQuote :: Text
sampleQuote = T.unwords
	[ "It was the best of times,"
	, "it was the worst of times,"
	, "it was the age of wisdom,"
	, "it was the age of foolishness."
	]

#else

#endif

pairPage :: Widget -> Handler RepHtml
pairPage = page "Pairing" (Just Configuration)

noPairing :: Text -> Handler RepHtml
noPairing pairingtype = pairPage $
	$(widgetFile "configurators/pairing/disabled")
