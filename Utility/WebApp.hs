{- Yesod webapp
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings, CPP, RankNTypes #-}

module Utility.WebApp where

import Common
import Utility.Tmp
import Utility.FileMode
import Utility.Hash

import qualified Yesod
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import Network.Socket
import "crypto-api" Crypto.Random
import qualified Web.ClientSession as CS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Blaze.ByteString.Builder (Builder)
import Control.Arrow ((***))
import Control.Concurrent
import Data.SecureMem
import Data.Byteable
#ifdef __ANDROID__
import Data.Endian
#endif

localhost :: HostName
localhost = "localhost"

{- Builds a command to use to start or open a web browser showing an url. -}
browserProc :: String -> CreateProcess
#ifdef darwin_HOST_OS
browserProc url = proc "open" [url]
#else
#ifdef __ANDROID__
-- Warning: The `am` command does not work very reliably on Android.
browserProc url = proc "am"
	["start", "-a", "android.intent.action.VIEW", "-d", url]
#else
#ifdef mingw32_HOST_OS
-- Warning: On Windows, no quoting or escaping of the url seems possible,
-- so spaces in it will cause problems. One approach is to make the url
-- be a relative filename, and adjust the returned CreateProcess to change
-- to the directory it's in.
browserProc url = proc "cmd" ["/c start " ++ url]
#else
browserProc url = proc "xdg-open" [url]
#endif
#endif
#endif

{- Binds to a socket on localhost, or possibly a different specified
 - hostname or address, and runs a webapp on it.
 -
 - An IO action can also be run, to do something with the address,
 - such as start a web browser to view the webapp.
 -}
runWebApp :: Maybe TLSSettings -> Maybe HostName -> Wai.Application -> (SockAddr -> IO ()) -> IO ()
runWebApp tlssettings h app observer = withSocketsDo $ do
	sock <- getSocket h
	void $ forkIO $ go webAppSettings sock app	
	sockaddr <- fixSockAddr <$> getSocketName sock
	observer sockaddr
  where
	go = (maybe runSettingsSocket (\ts -> runTLSSocket ts) tlssettings)

fixSockAddr :: SockAddr -> SockAddr
#ifdef __ANDROID__
{- On Android, the port is currently incorrectly returned in network
 - byte order, which is wrong on little endian systems. -}
fixSockAddr (SockAddrInet (PortNum port) addr) = SockAddrInet (PortNum $ swapEndian port) addr
#endif
fixSockAddr addr = addr

-- disable buggy sloworis attack prevention code
webAppSettings :: Settings

webAppSettings = setTimeout halfhour defaultSettings
  where
	halfhour = 30 * 60

{- Binds to a local socket, or if specified, to a socket on the specified
 - hostname or address. Selects any free port, unless the hostname ends with
 - ":port"
 -
 - Prefers to bind to the ipv4 address rather than the ipv6 address
 - of localhost, if it's available.
 -}
getSocket :: Maybe HostName -> IO Socket
getSocket h = do
#if defined(__ANDROID__) || defined (mingw32_HOST_OS)
	-- getAddrInfo currently segfaults on Android.
	-- The HostName is ignored by this code.
	when (isJust h) $
		error "getSocket with HostName not supported on this OS"
	addr <- inet_addr "127.0.0.1"
	sock <- socket AF_INET Stream defaultProtocol
	preparesocket sock
	bindSocket sock (SockAddrInet aNY_PORT addr)
	use sock
  where
#else
	addrs <- getAddrInfo (Just hints) (Just hostname) Nothing
	case (partition (\a -> addrFamily a == AF_INET) addrs) of
		(v4addr:_, _) -> go v4addr
		(_, v6addr:_) -> go v6addr
		_ -> error "unable to bind to a local socket"
  where
	hostname = fromMaybe localhost h
	hints = defaultHints { addrSocketType = Stream }
	{- Repeated attempts because bind sometimes fails for an
	 - unknown reason on OSX. -} 
	go addr = go' 100 addr
	go' :: Int -> AddrInfo -> IO Socket
	go' 0 _ = error "unable to bind to local socket"
	go' n addr = do
		r <- tryIO $ bracketOnError (open addr) close (useaddr addr)
		either (const $ go' (pred n) addr) return r
	open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	useaddr addr sock = do
		preparesocket sock
		bind sock (addrAddress addr)
		use sock
#endif
	preparesocket sock = setSocketOption sock ReuseAddr 1
	use sock = do
		listen sock maxListenQueue
		return sock

lookupRequestField :: CI.CI B.ByteString -> Wai.Request -> B.ByteString
lookupRequestField k req = fromMaybe "" . lookup k $ Wai.requestHeaders req

{- Rather than storing a session key on disk, use a random key
 - that will only be valid for this run of the webapp. -}
webAppSessionBackend :: Yesod.Yesod y => y -> IO (Maybe Yesod.SessionBackend)
webAppSessionBackend _ = do
	g <- newGenIO :: IO SystemRandom
	case genBytes 96 g of
		Left e -> error $ "failed to generate random key: " ++ show e
		Right (s, _) -> case CS.initKey s of
			Left e -> error $ "failed to initialize key: " ++ show e
			Right key -> use key
  where
	timeout = 120 * 60 -- 120 minutes
	use key =
		Just . Yesod.clientSessionBackend key . fst
			<$> Yesod.clientSessionDateCacher timeout

type AuthToken = SecureMem

toAuthToken :: T.Text -> AuthToken
toAuthToken = secureMemFromByteString . TE.encodeUtf8

fromAuthToken :: AuthToken -> T.Text
fromAuthToken = TE.decodeLatin1 . toBytes

{- Generates a random sha2_512 string, encapsulated in a SecureMem,
 - suitable to be used for an authentication secret. -}
genAuthToken :: IO AuthToken
genAuthToken = do
	g <- newGenIO :: IO SystemRandom
	return $
		case genBytes 512 g of
			Left e -> error $ "failed to generate auth token: " ++ show e
			Right (s, _) -> toAuthToken $ T.pack $ show $ sha2_512 $ L.fromChunks [s]

{- A Yesod isAuthorized method, which checks the auth cgi parameter
 - against a token extracted from the Yesod application.
 -
 - Note that the usual Yesod error page is bypassed on error, to avoid
 - possibly leaking the auth token in urls on that page!
 -
 - If the predicate does not match the route, the auth parameter is not
 - needed.
 -}
checkAuthToken :: Yesod.MonadHandler m => Yesod.RenderRoute site => (Yesod.HandlerSite m -> AuthToken) -> Yesod.Route site -> ([T.Text] -> Bool) -> m Yesod.AuthResult
checkAuthToken extractAuthToken r predicate
	| not (predicate (fst (Yesod.renderRoute r))) = return Yesod.Authorized
	| otherwise = do
		webapp <- Yesod.getYesod
		req <- Yesod.getRequest
		let params = Yesod.reqGetParams req
		if (toAuthToken <$> lookup "auth" params) == Just (extractAuthToken webapp)
			then return Yesod.Authorized
			else Yesod.sendResponseStatus unauthorized401 ()

{- A Yesod joinPath method, which adds an auth cgi parameter to every
 - url matching a predicate, containing a token extracted from the
 - Yesod application.
 - 
 - A typical predicate would exclude files under /static.
 -}
insertAuthToken :: forall y. (y -> AuthToken)
	-> ([T.Text] -> Bool)
	-> y
	-> T.Text
	-> [T.Text]
	-> [(T.Text, T.Text)]
	-> Builder
insertAuthToken extractAuthToken predicate webapp root pathbits params =
	fromText root `mappend` encodePath pathbits' encodedparams
  where
	pathbits' = if null pathbits then [T.empty] else pathbits
	encodedparams = map (TE.encodeUtf8 *** go) params'
	go "" = Nothing
	go x = Just $ TE.encodeUtf8 x
	authparam = (T.pack "auth", fromAuthToken (extractAuthToken webapp))
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
