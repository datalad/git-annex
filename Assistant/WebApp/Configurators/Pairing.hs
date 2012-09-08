{- git-annex assistant webapp configurator for pairing
 -
 - Pairing works like this:
 -
 - * The user opens StartPairR, which prompts them for a secret.
 - * The user submits it. A PairReq is broadcast out. The secret is
 -   stashed away in a list of known pairing secrets.
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
 -   up. Note that multiple other devices could also send PairAcks, and
 -   as long as they're valid, all those devices are paired with.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Common
import Assistant.Pairing
import Utility.Verifiable
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Data.Char
import System.Posix.User

getStartPairR :: Handler RepHtml
getStartPairR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Pairing"
	promptSecret Nothing $ error "TODO"

getFinishPairR :: PairReq -> Handler RepHtml
getFinishPairR req = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Pairing"
	promptSecret (Just req) $ error "TODO"

data InputSecret = InputSecret { secretText :: Maybe Text }

promptSecret :: Maybe PairReq -> Widget -> Widget
promptSecret req cont = do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $
			InputSecret <$> aopt textField "Secret phrase" Nothing
        case result of
                FormSuccess v -> do
			let secret = toSecret $ fromMaybe "" $ secretText v
			case req of
				Nothing -> case secretProblem secret of
					Nothing -> cont
					Just problem ->
						showform form enctype $ Just problem
				Just r ->
					if verified (fromPairReq r) secret
						then cont
						else showform form enctype $ Just
							"That's not the right secret phrase."
                _ -> showform form enctype Nothing
        where
                showform form enctype mproblem = do
			let start = isNothing req
			let badphrase = isJust mproblem
			let msg = fromMaybe "" mproblem
			let (username, hostname) = maybe ("", "")
				(\v -> (T.pack $ remoteUserName v, T.pack $ remoteHostName v))
				(verifiableVal . fromPairReq <$> req)
			u <- liftIO $ T.pack . userName
				<$> (getUserEntryForID =<< getEffectiveUserID)
			let sameusername = username == u
                        let authtoken = webAppFormAuthToken
                        $(widgetFile "configurators/pairing")

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
