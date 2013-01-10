{- Yesod webapp
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP, RankNTypes #-}

module Utility.WebApp where

import Common
import Utility.TempFile
import Utility.FileMode

import qualified Yesod
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
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Blaze.ByteString.Builder (Builder)
import Data.Monoid
import Control.Arrow ((***))
import Control.Concurrent

localhost :: String
localhost = "localhost"

{- Runs a web browser on a given url.
 -
 - Note: The url *will* be visible to an attacker. -}
runBrowser :: String -> (Maybe [(String, String)]) -> IO Bool
runBrowser url env = boolSystemEnv cmd [Param url] env
  where
#ifdef darwin_HOST_OS
	cmd = "open"
#else
	cmd = "xdg-open"
#endif

{- Binds to a socket on localhost, and runs a webapp on it.
 -
 - An IO action can also be run, to do something with the address,
 - such as start a web browser to view the webapp.
 -}
runWebApp :: Wai.Application -> (SockAddr -> IO ()) -> IO ()
runWebApp app observer = do
	sock <- localSocket
	void $ forkIO $ runSettingsSocket defaultSettings sock app
	observer =<< getSocketName sock

{- Binds to a local socket, selecting any free port.
 -
 - Prefers to bind to the ipv4 address rather than the ipv6 address
 - of localhost, if it's available.
 -
 - As a (very weak) form of security, only connections from 
 - localhost are accepted. -}
localSocket :: IO Socket
localSocket = do
	addrs <- getAddrInfo (Just hints) (Just localhost) Nothing
	case (partition (\a -> addrFamily a == AF_INET) addrs) of
		(v4addr:_, _) -> go v4addr
		(_, v6addr:_) -> go v6addr
		_ -> error "unable to bind to a local socket"
  where
	hints = defaultHints
		{ addrFlags = [AI_ADDRCONFIG]
		, addrSocketType = Stream
		}
	{- Repeated attempts because bind sometimes fails for an
	 - unknown reason on OSX. -} 
	go addr = go' 100 addr
	go' :: Int -> AddrInfo -> IO Socket
	go' 0 _ = error "unable to bind to local socket"
	go' n addr = do
		r <- tryIO $ bracketOnError (open addr) sClose (use addr)
		either (const $ go' (pred n) addr) return r
	open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
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

lookupRequestField :: CI.CI B.ByteString -> Wai.Request -> B.ByteString
lookupRequestField k req = fromMaybe "" . lookup k $ Wai.requestHeaders req

{- Rather than storing a session key on disk, use a random key
 - that will only be valid for this run of the webapp. -}
webAppSessionBackend :: Yesod.Yesod y => y -> IO (Maybe (Yesod.SessionBackend y))
webAppSessionBackend _ = do
	g <- newGenIO :: IO SystemRandom
	case genBytes 96 g of
		Left e -> error $ "failed to generate random key: " ++ show e
		Right (s, _) -> case CS.initKey s of
			Left e -> error $ "failed to initialize key: " ++ show e
			Right key -> return $ Just $
				Yesod.clientSessionBackend key 120

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
 - against a token extracted from the Yesod application.
 -
 - Note that the usual Yesod error page is bypassed on error, to avoid
 - possibly leaking the auth token in urls on that page!
 -}
checkAuthToken :: forall t sub. (t -> T.Text) -> Yesod.GHandler sub t Yesod.AuthResult
checkAuthToken extractToken = do
	webapp <- Yesod.getYesod
	req <- Yesod.getRequest
	let params = Yesod.reqGetParams req
	if lookup "auth" params == Just (extractToken webapp)
		then return Yesod.Authorized
		else Yesod.sendResponseStatus unauthorized401 ()

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

{- Creates a html shim file that's used to redirect into the webapp,
 - to avoid exposing the secret token when launching the web browser. -}
writeHtmlShim :: String -> String -> FilePath -> IO ()
writeHtmlShim title url file = viaTmp writeFileProtected file $ genHtmlShim title url

{- TODO: generate this static file using Yesod. -}
genHtmlShim :: String -> String -> String
genHtmlShim title url = unlines
	[ "<html>"
	, "<head>"
	, "<title>"++ title ++ "</title>"
	, "<meta http-equiv=\"refresh\" content=\"1; URL="++url++"\">"
	, "<body>"
	, "<p>"
	, "<a href=\"" ++ url ++ "\">" ++ title ++ "</a>"
	, "</p>"
	, "</body>"
	, "</html>"
	]
