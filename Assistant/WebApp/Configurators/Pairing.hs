{- git-annex assistant webapp configurator for pairing
 -
 - Copyright 2012,2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Pairing
import Assistant.WebApp.Common
import Annex.UUID
#ifdef WITH_PAIRING
import Assistant.DaemonStatus
import Assistant.Pairing.MakeRemote
import Assistant.Pairing.Network
import Assistant.Ssh
import Utility.Verifiable
#endif
import Utility.UserInfo
import Utility.Tor
import Assistant.WebApp.Pairing
import Assistant.Alert
import qualified Utility.MagicWormhole as Wormhole
import Assistant.MakeRemote
import Assistant.RemoteControl
import Assistant.Sync
import Assistant.WebApp.SideBar
import Command.P2P (unusedPeerRemoteName, PairingResult(..))
import P2P.Address
import Git
import Config.Files

import qualified Data.Map as M
import qualified Data.Text as T
#ifdef WITH_PAIRING
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.Char
import qualified Control.Exception as E
import Control.Concurrent
#endif
import Control.Concurrent.STM hiding (check)

getStartWormholePairFriendR :: Handler Html
getStartWormholePairFriendR = startWormholePairR PairingWithFriend

getStartWormholePairSelfR :: Handler Html
getStartWormholePairSelfR = startWormholePairR PairingWithSelf

startWormholePairR :: PairingWith -> Handler Html
startWormholePairR pairingwith = whenTorInstalled $ whenWormholeInstalled $
	pairPage $
		$(widgetFile "configurators/pairing/wormhole/start")

getPrepareWormholePairR :: PairingWith -> Handler Html
getPrepareWormholePairR pairingwith = do
	enableTor
	myaddrs <- liftAnnex loadP2PAddresses
	remotename <- liftAnnex unusedPeerRemoteName
	h <- liftAssistant $
		startWormholePairing pairingwith remotename myaddrs
	i <- liftIO . addWormholePairingState h
		=<< wormholePairingState <$> getYesod
	redirect $ RunningWormholePairR i

enableTor :: Handler ()
enableTor = do
	gitannex <- liftIO readProgramFile
	(transcript, ok) <- liftIO $ processTranscript gitannex ["enable-tor"] Nothing
	if ok
		-- Reload remotedameon so it's serving the tor hidden
		-- service.
		then liftAssistant $ sendRemoteControl RELOAD
		else giveup $ "Failed to enable tor\n\n" ++ transcript

getRunningWormholePairR :: WormholePairingId -> Handler Html
getRunningWormholePairR = runningWormholePairR

postRunningWormholePairR :: WormholePairingId -> Handler Html
postRunningWormholePairR = runningWormholePairR

runningWormholePairR :: WormholePairingId -> Handler Html
runningWormholePairR i = go =<< getWormholePairingHandle i
  where
	go Nothing = redirect StartWormholePairFriendR
	go (Just h) = pairPage $  withPairingWith h $ \pairingwith -> do
		ourcode <- liftIO $ getOurWormholeCode h
		let codeprompt = case pairingwith of
			PairingWithFriend -> "Your friend's pairing code"
			PairingWithSelf -> "The other device's pairing code"
		((result, form), enctype) <- liftH $
			runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $
				areq (checkwormholecode ourcode pairingwith textField) (bfs codeprompt) Nothing
		case result of
			FormSuccess t -> case Wormhole.toCode (T.unpack t) of
				Nothing -> giveup invalidcode
				Just theircode -> finish h theircode
			_ -> showform form enctype ourcode pairingwith
	
	showform form enctype ourcode pairingwith =
		$(widgetFile "configurators/pairing/wormhole/prompt")
	
	checkwormholecode ourcode pairingwith = check $ \t ->
		case Wormhole.toCode (T.unpack t) of
			Nothing -> Left (T.pack invalidcode)
			Just theircode
				| theircode == ourcode -> Left $
					case pairingwith of
						PairingWithSelf -> "Oops -- You entered this repository's pairing code. Enter the pairing code of the *other* repository."
						PairingWithFriend -> "Oops -- You entered your pairing code. Enter your friend's pairing code."
				| otherwise -> Right t

	invalidcode = "That does not look like a valid pairing code. Try again..."

	finish h theircode = do
		void $ liftIO $ sendTheirWormholeCode h theircode
		res <- liftAssistant $ finishWormholePairing h
		case res of
			SendFailed -> giveup "Failed sending data to pair."
			ReceiveFailed -> giveup "Failed receiving data from pair."
			LinkFailed e -> giveup $ "Failed linking to pair: " ++ e
			PairSuccess -> withRemoteName h $ \remotename -> do
				r <- liftAnnex $ addRemote (return remotename)
				liftAssistant $ syncRemote r
				liftAssistant $ sendRemoteControl RELOAD
				redirect DashboardR

getWormholePairingHandle :: WormholePairingId -> Handler (Maybe WormholePairingHandle)
getWormholePairingHandle i = do
	s <- wormholePairingState <$> getYesod
	liftIO $ atomically $ M.lookup i <$> readTVar s

whenTorInstalled :: Handler Html -> Handler Html
whenTorInstalled a = ifM (liftIO torIsInstalled)
	( a
	, page "Need Tor" (Just Configuration) $
		$(widgetFile "configurators/needtor")
	)

whenWormholeInstalled :: Handler Html -> Handler Html
whenWormholeInstalled a = ifM (liftIO Wormhole.isInstalled)
	( a
	, page "Need Magic Wormhole" (Just Configuration) $
		$(widgetFile "configurators/needmagicwormhole")
	)

{- Starts local pairing. -}
getStartLocalPairR :: Handler Html
getStartLocalPairR = postStartLocalPairR
postStartLocalPairR :: Handler Html
#ifdef WITH_PAIRING
postStartLocalPairR = promptSecret Nothing $
	startLocalPairing PairReq noop pairingAlert Nothing
#else
postStartLocalPairR = noLocalPairing

noLocalPairing :: Handler Html
noLocalPairing = noPairing "local"
#endif

{- Runs on the system that responds to a local pair request; sets up the ssh
 - authorized key first so that the originating host can immediately sync
 - with us. -}
getFinishLocalPairR :: PairMsg -> Handler Html
getFinishLocalPairR = postFinishLocalPairR
postFinishLocalPairR :: PairMsg -> Handler Html
#ifdef WITH_PAIRING
postFinishLocalPairR msg = promptSecret (Just msg) $ \_ secret -> do
	repodir <- liftH $ repoPath <$> liftAnnex gitRepo
	liftIO $ setup repodir
	startLocalPairing PairAck (cleanup repodir) alert uuid "" secret
  where
	alert = pairRequestAcknowledgedAlert (pairRepo msg) . Just
	setup repodir = setupAuthorizedKeys msg repodir
	cleanup repodir = removeAuthorizedKeys True repodir $
		remoteSshPubKey $ pairMsgData msg
	uuid = Just $ pairUUID $ pairMsgData msg
#else
postFinishLocalPairR _ = noLocalPairing
#endif

getRunningLocalPairR :: SecretReminder -> Handler Html
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
	urlrender <- liftH getUrlRender
	reldir <- fromJust . relDir <$> liftH getYesod

	sendrequests <- liftAssistant $ asIO2 $ mksendrequests urlrender
	{- Generating a ssh key pair can take a while, so do it in the
	 - background. -}
	thread <- liftAssistant $ asIO $ do
		keypair <- liftIO $ genSshKeyPair
		let pubkey = either error id $ validateSshPubKey $ sshPubKey keypair
		pairdata <- liftIO $ PairData
			<$> getHostname
			<*> (either error id <$> myUserName)
			<*> pure reldir
			<*> pure pubkey
			<*> (maybe genUUID return muuid)
		let sender = multicastPairMsg Nothing secret pairdata
		let pip = PairingInProgress secret Nothing keypair pairdata stage
		startSending pip stage $ sendrequests sender
	void $ liftIO $ forkIO thread

	liftH $ redirect $ RunningLocalPairR $ toSecretReminder displaysecret
  where
	{- Sends pairing messages until the thread is killed,
	 - and shows an activity alert while doing it.
	 -
	 - The cancel button returns the user to the DashboardR. This is
	 - not ideal, but they have to be sent somewhere, and could
	 - have been on a page specific to the in-process pairing
	 - that just stopped, so can't go back there.
	 -}
	mksendrequests urlrender sender _stage = do
		tid <- liftIO myThreadId
		let selfdestruct = AlertButton
			{ buttonLabel = "Cancel"
			, buttonPrimary = True
			, buttonUrl = urlrender DashboardR
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
promptSecret :: Maybe PairMsg -> (Text -> Secret -> Widget) -> Handler Html
promptSecret msg cont = pairPage $ do
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $
			InputSecret <$> aopt textField (bfs "Secret phrase") Nothing
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
		u <- liftIO myUserName
		let sameusername = Right username == (T.pack <$> u)
		$(widgetFile "configurators/pairing/local/prompt")

{- This counts unicode characters as more than one character,
 - but that's ok; they *do* provide additional entropy. -}
secretProblem :: Secret -> Maybe Text
secretProblem s
	| B.null s = Just "The secret phrase cannot be left empty. (Remember that punctuation and white space is ignored.)"
	| B.length s < 6 = Just "Enter a longer secret phrase, at least 6 characters, but really, a phrase is best! This is not a password you'll need to enter every day."
	| s == toSecret sampleQuote = Just "Speaking of foolishness, don't paste in the example I gave. Enter a different phrase, please!"
	| otherwise = Nothing

toSecret :: Text -> Secret
toSecret s = T.encodeUtf8 $ T.toLower $ T.filter isAlphaNum s

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

pairPage :: Widget -> Handler Html
pairPage = page "Pairing" (Just Configuration)

noPairing :: Text -> Handler Html
noPairing pairingtype = pairPage $
	$(widgetFile "configurators/pairing/disabled")
