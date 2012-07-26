{- Yesod webapp
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP, RankNTypes #-}

module Utility.WebApp where

import Common

import Yesod
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Control.Monad.IO.Class
import Network.HTTP.Types
import System.Log.Logger
import Data.ByteString.Lazy.UTF8
import qualified Data.CaseInsensitive as CI
import Network.Socket
import Control.Exception
import Crypto.Random
import Data.Digest.Pure.SHA
import qualified Web.ClientSession as CS
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Blaze.ByteString.Builder (Builder)
import Data.Monoid
import Control.Arrow ((***))

localhost :: String
localhost = "localhost"

{- Runs a web browser on a given url.
 -
 - Note: The url *will* be visible to an attacker. -}
runBrowser :: String -> IO Bool
runBrowser url = boolSystem cmd [Param url]
	where
#if MAC
		cmd = "open"
#else
		cmd = "xdg-open"
#endif

{- Binds to a socket on localhost, and runs a webapp on it.
 -
 - An IO action can also be run, to do something with the port number,
 - such as start a web browser to view the webapp.
  -}
runWebApp :: Application -> (PortNumber -> IO ()) -> IO ()
runWebApp app observer = do
	sock <- localSocket
	observer =<< socketPort sock
	runSettingsSocket defaultSettings sock app

{- Binds to a local socket, selecting any free port.
 -
 - As a (very weak) form of security, only connections from 
 - localhost are accepted. -}
localSocket :: IO Socket
localSocket = do
	addrs <- getAddrInfo (Just hints) (Just localhost) Nothing
	go $ Prelude.head addrs
	where
		hints = defaultHints
			{ addrFlags = [AI_ADDRCONFIG, AI_NUMERICSERV]
			, addrSocketType = Stream
			}
		go addr = bracketOnError (open addr) close (use addr)
		open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		close = sClose
		use addr sock = do
			setSocketOption sock ReuseAddr 1
			bindSocket sock (addrAddress addr)
			listen sock maxListenQueue
			return sock

{- Checks if debugging is actually enabled. -}
debugEnabled :: IO Bool
debugEnabled = do
	l <- getRootLogger
	return $ getLevel l <= Just DEBUG

{- WAI middleware that logs using System.Log.Logger at debug level.
 -
 - Recommend only inserting this middleware when debugging is actually
 - enabled, as it's not optimised at all.
 -}
httpDebugLogger :: Wai.Middleware
httpDebugLogger waiApp req = do
	logRequest req
	waiApp req

logRequest :: MonadIO m => Wai.Request -> m ()
logRequest req = do
	liftIO $ debugM "WebApp" $ unwords
		[ showSockAddr $ Wai.remoteHost req
		, frombs $ Wai.requestMethod req
		, frombs $ Wai.rawPathInfo req
		--, show $ Wai.httpVersion req
		--, frombs $ lookupRequestField "referer" req
		, frombs $ lookupRequestField "user-agent" req
		]
	where
		frombs v = toString $ L.fromChunks [v]

lookupRequestField :: CI.CI Ascii -> Wai.Request -> Ascii
lookupRequestField k req = fromMaybe "" . lookup k $ Wai.requestHeaders req

{- Rather than storing a session key on disk, use a random key
 - that will only be valid for this run of the webapp. -}
webAppSessionBackend :: Yesod y => y -> IO (Maybe (SessionBackend y))
webAppSessionBackend _ = do
	g <- newGenIO :: IO SystemRandom
	case genBytes 96 g of
		Left e -> error $ "failed to generate random key: " ++ show e
		Right (s, _) -> case CS.initKey s of
			Left e -> error $ "failed to initialize key: " ++ show e
			Right key -> return $ Just $
				clientSessionBackend key 120

{- Generates a random sha512 string, suitable to be used for an
 - authentication secret. -}
genRandomToken :: IO String
genRandomToken = do
	g <- newGenIO :: IO SystemRandom
	return $
		case genBytes 512 g of
			Left e -> error $ "failed to generate secret token: " ++ show e
			Right (s, _) -> showDigest $ sha512 $ L.fromChunks [s]

{- A Yesod isAuthorized method, which checks the auth cgi parameter
 - against a token extracted from the Yesod application. -}
checkAuthToken :: forall t sub. (t -> T.Text) -> GHandler sub t AuthResult
checkAuthToken extractToken = do
	webapp <- getYesod
	req <- getRequest
	let params = reqGetParams req
	if lookup "auth" params == Just (extractToken webapp)
		then return Authorized
		else return AuthenticationRequired

{- A Yesod joinPath method, which adds an auth cgi parameter to every
 - url matching a predicate, containing a token extracted from the
 - Yesod application.
 - 
 - A typical predicate would exclude files under /static.
 -}
insertAuthToken :: forall y. (y -> T.Text)
	-> ([T.Text] -> Bool)
	-> y
	-> T.Text
	-> [T.Text]
	-> [(T.Text, T.Text)]
	-> Builder
insertAuthToken extractToken predicate webapp root pathbits params =
	fromText root `mappend` encodePath pathbits' encodedparams
	where
		pathbits' = if null pathbits then [T.empty] else pathbits
		encodedparams = map (TE.encodeUtf8 *** go) params'
		go "" = Nothing
		go x = Just $ TE.encodeUtf8 x
		authparam = (T.pack "auth", extractToken webapp)
		params'
			| predicate pathbits = authparam:params
			| otherwise = params
