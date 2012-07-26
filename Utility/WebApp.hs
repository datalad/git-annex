{- WAI webapp
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP #-}

module Utility.WebApp where

import Common

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Control.Monad.IO.Class
import Network.HTTP.Types
import System.Log.Logger
import Data.ByteString.Lazy.UTF8
import Data.ByteString.Lazy
import Data.CaseInsensitive as CI
import Network.Socket
import Control.Exception

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
httpDebugLogger :: Middleware
httpDebugLogger waiApp req = do
	logRequest req
	waiApp req

logRequest :: MonadIO m => Request -> m ()
logRequest req = do
	liftIO $ debugM "WebApp" $ unwords
		[ showSockAddr $ remoteHost req
		, frombs $ requestMethod req
		, frombs $ rawPathInfo req
		--, show $ httpVersion req
		--, frombs $ lookupRequestField "referer" req
		, frombs $ lookupRequestField "user-agent" req
		]
	where
		frombs v = toString $ fromChunks [v]

lookupRequestField :: CI Ascii -> Request -> Ascii
lookupRequestField k req = fromMaybe "" . lookup k $ requestHeaders req
