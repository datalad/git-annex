{- git-annex assistant webapp configurator for pairing
 -
 - Pairing works like this:
 -
 - * The user opens StartPairR, which prompts them for a secret.
 - * The user submits it. The pairing secret is stored for later.
 -   A PairReq is broadcast out.
 - * On another device, it's received, and that causes its webapp to 
 -   display an Alert.
 - * The user there clicks the button, which opens FinishPairR,
 -   which prompts them for the same secret.
 - * The secret is used to verify the PairReq. If it checks out,
 -   a PairAck is sent, and the other device adds the ssh key from the
 -   PairReq to its authorized_keys, and sets up the remote.
 - * The PairAck is received back at the device that started the process.
 -   It's verified using the stored secret. The ssh key from the PairAck
 -   is added. An Alert is displayed noting that the pairing has been set
 -   up. The pairing secret is removed to prevent anyone cracking the
 -   crypto. Syncing starts. A PairDone is sent.
 - * The PairDone is received, and an alert shown indicating pairing is
 -   done.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Pairing
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod
#ifdef WITH_PAIRING
import Assistant.Common
import Assistant.Pairing.Network
import Assistant.Ssh
import Assistant.Alert
import Assistant.DaemonStatus
import Utility.Verifiable
import Utility.Network
#endif

import Yesod
import Data.Text (Text)
#ifdef WITH_PAIRING
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Data.Char
import System.Posix.User
import qualified Control.Exception as E
import Control.Concurrent
#endif

getStartPairR :: Handler RepHtml
#ifdef WITH_PAIRING
getStartPairR = promptSecret Nothing $ startPairing PairReq noop
#else
getStartPairR = noPairing
#endif

getFinishPairR :: PairMsg -> Handler RepHtml
#ifdef WITH_PAIRING
getFinishPairR msg = promptSecret (Just msg) $ \_ secret -> do
	setup
	startPairing PairAck cleanup "" secret
	where
		pubkey = remoteSshPubKey $ pairMsgData msg
		setup  = do
			liftIO $ validateSshPubKey pubkey
			unlessM (liftIO $ makeAuthorizedKeys False pubkey) $
				error "failed setting up ssh authorized keys"
		cleanup = error "TODO clean up authorized keys and generated ssh key and remove git remote"
#else
getFinishPairR _ = noPairing
#endif

getInprogressPairR :: Text -> Handler RepHtml
#ifdef WITH_PAIRING
getInprogressPairR secret = pairPage $ do
	$(widgetFile "configurators/pairing/inprogress")
#else
getInprogressPairR _ = noPairing
#endif

#ifdef WITH_PAIRING

{- Starts pairing, at either the PairReq (initiating host) or 
 - PairAck (responding host) stage.
 -
 - Displays an alert, and starts a thread sending the pairing message,
 - which will continue running until the other host responds, or until
 - canceled by the user. If canceled by the user, runs the oncancel action.
 -
 - Redirects to the pairing in progress page.
 -}
startPairing :: PairStage -> IO () -> Text -> Secret -> Widget
startPairing stage oncancel displaysecret secret = do
	keypair <- liftIO $ genSshKeyPair
	dstatus <- daemonStatus <$> lift getYesod
	urlrender <- lift getUrlRender
	let homeurl = urlrender HomeR
	pairdata <- PairData
		<$> liftIO getHostname
		<*> liftIO getUserName
		<*> (fromJust . relDir <$> lift getYesod)
		<*> pure (sshPubKey keypair)
	liftIO $ do
		let sender = multicastPairMsg Nothing secret stage pairdata
		let pip = PairingInProgress secret Nothing keypair pairdata
		startSending dstatus pip $ sendrequests sender dstatus homeurl
	lift $ redirect $ InprogressPairR displaysecret
	where
		{- Sends pairing messages until the thread is killed,
		 - and shows an activity alert while doing it.
		 -
		 - The cancel button returns the user to the HomeR. This is
		 - not ideal, but they have to be sent somewhere, and could
		 - have been on a page specific to the in-process pairing
		 - that just stopped, so can't go back there.
		 -}
		sendrequests sender dstatus homeurl = do
			tid <- myThreadId
			let selfdestruct = AlertButton
				{ buttonLabel = "Cancel"
				, buttonUrl = homeurl
				, buttonAction = Just $ const $ do
					oncancel
					killThread tid
				}
			alertDuring dstatus (pairingAlert selfdestruct) $ do
				_ <- E.try sender :: IO (Either E.SomeException ())
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
			u <- T.pack <$> liftIO getUserName
			let sameusername = username == u
                        let authtoken = webAppFormAuthToken
                        $(widgetFile "configurators/pairing/prompt")

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

getUserName :: IO String
getUserName = userName <$> (getUserEntryForID =<< getEffectiveUserID)

pairPage :: Widget -> Handler RepHtml
pairPage w = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Pairing"
	w

{- From Dickens -}
sampleQuote :: Text
sampleQuote = T.unwords
	[ "It was the best of times,"
	, "it was the worst of times,"
	, "it was the age of wisdom,"
	, "it was the age of foolishness."
	]

#else

noPairing :: Handler RepHtml
noPairing = pairPage $
	$(widgetFile "configurators/pairing/disabled")

#endif
