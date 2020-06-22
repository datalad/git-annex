{- | Restricted `ManagerSettings` for <https://haskell-lang.org/library/http-client>
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Portions from http-client-tls Copyright (c) 2013 Michael Snoyman
 -
 - License: MIT
 -}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, LambdaCase, PatternGuards #-}

-- This is a vendored copy of Network.HTTP.Client.Restricted from the
-- http-client-restricted package, and will be removed once that package
-- is available in all build environments.
module Utility.HttpManagerRestricted (
	Restriction,
	checkAddressRestriction,
	addressRestriction,
	mkRestrictedManagerSettings,
	ConnectionRestricted(..),
	connectionRestricted,
	ProxyRestricted(..),
	IPAddrString,
) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal (ManagerSettings(..), Connection, runProxyOverride)
import Network.HTTP.Client.TLS (mkManagerSettingsContext)
import Network.Socket
import Network.BSD (getProtocolNumber)
import Control.Exception
import qualified Network.Connection as NC
import qualified Data.ByteString.UTF8 as BU
import Data.Maybe
import Data.Default
import Data.Typeable
import qualified Data.Semigroup as Sem
import Data.Monoid
import Control.Applicative
import Prelude

-- | Configuration of which HTTP connections to allow and which to
-- restrict.
data Restriction = Restriction
	{ checkAddressRestriction :: AddrInfo -> Maybe ConnectionRestricted
	}

-- | Decide if a HTTP connection is allowed based on the IP address
-- of the server.
--
-- After the restriction is checked, the same IP address is used
-- to connect to the server. This avoids DNS rebinding attacks
-- being used to bypass the restriction.
--
-- > myRestriction :: Restriction
-- > myRestriction = addressRestriction $ \addr ->
-- >	if isPrivateAddress addr
-- >		then Just $ connectionRestricted
-- >			("blocked connection to private IP address " ++)
-- > 		else Nothing
addressRestriction :: (AddrInfo -> Maybe ConnectionRestricted) -> Restriction
addressRestriction f = mempty { checkAddressRestriction = f }

appendRestrictions :: Restriction -> Restriction -> Restriction
appendRestrictions a b = Restriction
	{ checkAddressRestriction = \addr ->
		checkAddressRestriction a addr <|> checkAddressRestriction b addr
	}

-- | mempty does not restrict HTTP connections in any way
instance Monoid Restriction where
	mempty = Restriction
		{ checkAddressRestriction = \_ -> Nothing
		}

instance Sem.Semigroup Restriction where
	(<>) = appendRestrictions

-- | Value indicating that a connection was restricted, and giving the
-- reason why.
data ConnectionRestricted = ConnectionRestricted String
	deriving (Show, Typeable)

instance Exception ConnectionRestricted

-- | A string containing an IP address, for display to a user.
type IPAddrString = String

-- | Constructs a ConnectionRestricted, passing the function a string
-- containing the IP address of the HTTP server.
connectionRestricted :: (IPAddrString -> String) -> AddrInfo -> ConnectionRestricted
connectionRestricted mkmessage = 
	ConnectionRestricted . mkmessage . showSockAddress . addrAddress

-- | Value indicating that the http proxy will not be used.
data ProxyRestricted = ProxyRestricted
	deriving (Show)

-- Adjusts a ManagerSettings to enforce a Restriction. The restriction
-- will be checked each time a Request is made, and for each redirect
-- followed.
--
-- This overrides the `managerRawConnection`
-- and `managerTlsConnection` with its own implementations that check 
-- the Restriction. They should otherwise behave the same as the
-- ones provided by http-client-tls.
--
-- This function is not exported, because using it with a ManagerSettings
-- produced by something other than http-client-tls would result in
-- surprising behavior, since its connection methods would not be used.
--
-- The http proxy is also checked against the Restriction, and if
-- access to it is blocked, the http proxy will not be used.
restrictManagerSettings
	:: Maybe NC.ConnectionContext
	-> Maybe NC.TLSSettings
	-> Restriction
	-> ManagerSettings
	-> IO (ManagerSettings, Maybe ProxyRestricted)
restrictManagerSettings mcontext mtls cfg base = restrictProxy cfg $ base
	{ managerRawConnection = restrictedRawConnection cfg
	, managerTlsConnection = restrictedTlsConnection mcontext mtls cfg
	, managerWrapException = wrapOurExceptions base
	}

-- | Makes a TLS-capable ManagerSettings with a Restriction applied to it.
--
-- The Restriction will be checked each time a Request is made, and for
-- each redirect followed.
--
-- Aside from checking the Restriction, it should behave the same as
-- `Network.HTTP.Client.TLS.mkManagerSettingsContext`
-- from http-client-tls.
--
-- > main = do
-- > 	manager <- newManager . fst 
-- > 		=<< mkRestrictedManagerSettings myRestriction Nothing Nothing
-- >	request <- parseRequest "http://httpbin.org/get"
-- > 	response <- httpLbs request manager
-- > 	print $ responseBody response
--
-- The HTTP proxy is also checked against the Restriction, and will not be
-- used if the Restriction does not allow it. Just ProxyRestricted
-- is returned when the HTTP proxy has been restricted.
-- 
-- See `mkManagerSettingsContext` for why
-- it can be useful to provide a `NC.ConnectionContext`.
-- 
-- Note that SOCKS is not supported.
mkRestrictedManagerSettings
	:: Restriction
	-> Maybe NC.ConnectionContext
	-> Maybe NC.TLSSettings
	-> IO (ManagerSettings, Maybe ProxyRestricted)
mkRestrictedManagerSettings cfg mcontext mtls =
	restrictManagerSettings mcontext mtls cfg $
		mkManagerSettingsContext mcontext (fromMaybe def mtls) Nothing

restrictProxy
	:: Restriction
	-> ManagerSettings
	-> IO (ManagerSettings, Maybe ProxyRestricted)
restrictProxy cfg base = do	
	http_proxy_addr <- getproxyaddr False
	https_proxy_addr <- getproxyaddr True
	let (http_proxy, http_r) = mkproxy http_proxy_addr
	let (https_proxy, https_r) = mkproxy https_proxy_addr
	let ms = managerSetInsecureProxy http_proxy $ 
		managerSetSecureProxy https_proxy base
	return (ms, http_r <|> https_r)
  where
	-- This does not use localhost because http-client may choose
	-- not to use the proxy for localhost.
	testnetip = "198.51.100.1"
	dummyreq https = parseRequest_ $
		"http" ++ (if https then "s" else "") ++ "://" ++ testnetip

	getproxyaddr https = extractproxy >>= \case
		Nothing -> return Nothing
		Just p -> do
			proto <- getProtocolNumber "tcp"
			let serv = show (proxyPort p)
			let hints = defaultHints
				{ addrFlags = [AI_ADDRCONFIG]
				, addrProtocol = proto
				, addrSocketType = Stream
				}
			let h = BU.toString $ proxyHost p
			getAddrInfo (Just hints) (Just h) (Just serv) >>= \case
				[] -> return Nothing
				(addr:_) -> return $ Just addr
	  where
		-- These contortions are necessary until this issue
		-- is fixed:
		-- https://github.com/snoyberg/http-client/issues/355
		extractproxy = do
			let po = if https
				then managerProxySecure base
				else managerProxyInsecure base
			f <- runProxyOverride po https
			return $ proxy $ f $ dummyreq https
	
	mkproxy Nothing = (noProxy, Nothing)
	mkproxy (Just proxyaddr) = case checkAddressRestriction cfg proxyaddr of
		Nothing -> (addrtoproxy (addrAddress proxyaddr), Nothing)
		Just _ -> (noProxy, Just ProxyRestricted)
	
	addrtoproxy addr = case addr of
		SockAddrInet pn _ -> mk pn
		SockAddrInet6 pn _ _ _ -> mk pn
		_ -> noProxy
	  where
		mk pn = useProxy Network.HTTP.Client.Proxy
			{ proxyHost = BU.fromString (showSockAddress addr)
			, proxyPort = fromIntegral pn
			}

wrapOurExceptions :: ManagerSettings -> Request -> IO a -> IO a
wrapOurExceptions base req a =
	let wrapper se
		| Just (_ :: ConnectionRestricted) <- fromException se = 
	         	toException $ HttpExceptionRequest req $
				InternalException se
		| otherwise = se
	 in managerWrapException base req (handle (throwIO . wrapper) a)

restrictedRawConnection :: Restriction -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
restrictedRawConnection cfg = getConnection cfg Nothing Nothing

restrictedTlsConnection :: Maybe NC.ConnectionContext -> Maybe NC.TLSSettings -> Restriction -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
restrictedTlsConnection mcontext mtls cfg = 
	getConnection cfg (Just (fromMaybe def mtls)) mcontext

-- Based on Network.HTTP.Client.TLS.getTlsConnection.
--
-- Checks the Restriction
--
-- Does not support SOCKS.
getConnection
	:: Restriction
	-> Maybe NC.TLSSettings
	-> Maybe NC.ConnectionContext
	-> IO (Maybe HostAddress -> String -> Int -> IO Connection)
getConnection cfg tls mcontext = do
	context <- maybe NC.initConnectionContext return mcontext
	return $ \_ha h p -> bracketOnError
		(go context h p)
		NC.connectionClose
		convertConnection
   where
	go context h p = do
		let connparams = NC.ConnectionParams
			{ NC.connectionHostname = h
			, NC.connectionPort = fromIntegral p
			, NC.connectionUseSecure = tls
			, NC.connectionUseSocks = Nothing -- unsupprted
			}
		proto <- getProtocolNumber "tcp"
		let serv = show p
		let hints = defaultHints
			{ addrFlags = [AI_ADDRCONFIG]
			, addrProtocol = proto
			, addrSocketType = Stream
			}
		addrs <- getAddrInfo (Just hints) (Just h) (Just serv)
		bracketOnError
			(firstSuccessful $ map tryToConnect addrs)
			close
			(\sock -> NC.connectFromSocket context sock connparams)
	  where
		tryToConnect addr = case checkAddressRestriction cfg addr of
			Nothing -> bracketOnError
				(socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
				close
				(\sock -> connect sock (addrAddress addr) >> return sock)
			Just r -> throwIO r
		firstSuccessful [] = throwIO $ NC.HostNotResolved h
		firstSuccessful (a:as) = a `catch` \(e ::IOException) ->
			case as of
				[] -> throwIO e
				_ -> firstSuccessful as

-- Copied from Network.HTTP.Client.TLS, unfortunately not exported.
convertConnection :: NC.Connection -> IO Connection
convertConnection conn = makeConnection
    (NC.connectionGetChunk conn)
    (NC.connectionPut conn)
    -- Closing an SSL connection gracefully involves writing/reading
    -- on the socket.  But when this is called the socket might be
    -- already closed, and we get a @ResourceVanished@.
    (NC.connectionClose conn `Control.Exception.catch` \(_ :: IOException) -> return ())

-- For ipv4 and ipv6, the string will contain only the IP address,
-- omitting the port that the Show instance includes.
showSockAddress :: SockAddr -> IPAddrString
showSockAddress a@(SockAddrInet _ _) =
	takeWhile (/= ':') $ show a
showSockAddress a@(SockAddrInet6 _ _ _ _) =
	takeWhile (/= ']') $ drop 1 $ show a
showSockAddress a = show a
