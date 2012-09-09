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
 -   PairReq. An Alert is displayed noting that the pairing has been set up.
 - * The PairAck is received back at the device that started the process.
 -   It's verified using the stored secret. The ssh key from the PairAck
 -   is added. An Alert is displayed noting that the pairing has been set
 -   up. The pairing secret is removed to prevent anyone cracking the
 -   crypto.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Pairing
#ifdef WITH_PAIRING
import Assistant.Pairing.Network
import Assistant.Common
import Assistant.Alert
import Assistant.DaemonStatus
import Utility.Verifiable
import Utility.Network
#endif
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod

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
getStartPairR = promptSecret Nothing $ \rawsecret secret -> do
	dstatus <- daemonStatus <$> lift getYesod
	urlrender <- lift getUrlRender
	let homeurl = urlrender HomeR
	hostname <- liftIO $ getHostname
	username <- liftIO $ getUserName
	reldir <- fromJust . relDir <$> lift getYesod
	let sshkey = "" -- TODO generate/read ssh key
	let mkmsg addr = PairMsg $ mkVerifiable
		(PairReq, PairData hostname addr username reldir sshkey) secret
	liftIO $ do
		pip <- PairingInProgress secret
			<$> sendrequests mkmsg dstatus homeurl
		oldpip <- modifyDaemonStatus dstatus $
			\s -> (s { pairingInProgress = Just pip }, pairingInProgress s)
		maybe noop stopold oldpip
	lift $ redirect $ InprogressPairR rawsecret
	where
		{- Sends pairing messages until the thread is killed,
		 - and shows an activity alert while doing it.
		 -
		 - The button returns the user to the HomeR. This is
		 - not ideal, but they have to be sent somewhere, and could
		 - have been on a page specific to the in-process pairing
		 - that just stopped.
		 -}
		sendrequests mkmsg dstatus homeurl = forkIO $ do
			tid <- myThreadId
			let selfdestruct = AlertButton
				{ buttonLabel = "Cancel"
				, buttonUrl = homeurl
				, buttonAction = Just $ const $ killThread tid
				}
			alertDuring dstatus (pairRequestAlert selfdestruct) $ do
				_ <- E.try (multicastPairMsg mkmsg) :: IO (Either E.SomeException ())
				return ()
		stopold = killThread  . inProgressThreadId
#else
getStartPairR = noPairing
#endif

getInprogressPairR :: Text -> Handler RepHtml
#ifdef WITH_PAIRING
getInprogressPairR secret = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Pairing"
	$(widgetFile "configurators/pairing/inprogress")
#else
getInprogressPairR _ = noPairing
#endif

getFinishPairR :: PairMsg -> Handler RepHtml
#ifdef WITH_PAIRING
getFinishPairR msg = promptSecret (Just msg) $ \_ secret -> do
	error "TODO"
#else
getFinishPairR _ = noPairing
#endif

#ifdef WITH_PAIRING
data InputSecret = InputSecret { secretText :: Maybe Text }

promptSecret :: Maybe PairMsg -> (Text -> Secret -> Widget) -> Handler RepHtml
promptSecret msg cont = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Pairing"
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
					if verified (fromPairMsg m) secret
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
				(\(_, v) -> (T.pack $ remoteUserName v, T.pack $ fromMaybe (showAddr $ remoteAddress v) (remoteHostName v)))
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

{- From Dickens -}
sampleQuote :: Text
sampleQuote = T.unwords
	[ "It was the best of times,"
	, "it was the worst of times,"
	, "it was the age of wisdom,"
	, "it was the age of foolishness."
	]

getUserName :: IO String
getUserName = userName <$> (getUserEntryForID =<< getEffectiveUserID)

#else

noPairing :: Handler RepHtml
noPairing = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Pairing"
	$(widgetFile "configurators/pairing/disabled")

#endif
