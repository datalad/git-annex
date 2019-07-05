{- | Restricted Manager for http-client-tls
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 - 
 - Portions from http-client-tls Copyright (c) 2013 Michael Snoyman
 -
 - License: MIT
 -}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, LambdaCase, PatternGuards #-}

module Utility.HttpManagerRestricted (
	restrictManagerSettings,
	Restriction(..),
	ConnectionRestricted(..),
	addrConnectionRestricted,
	ProxyRestricted(..),
	IPAddrString,
) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal
	(ManagerSettings(..), Connection, runProxyOverride, makeConnection)
import Network.Socket
import Network.BSD (getProtocolNumber)
import Control.Exception
import qualified Network.Connection as NC
import qualified Data.ByteString.UTF8 as BU
import Data.Default
import Data.Typeable
import Control.Applicative
import qualified Data.Semigroup as Sem
import Data.Monoid
import Prelude

data Restriction = Restriction
	{ checkAddressRestriction :: AddrInfo -> Maybe ConnectionRestricted
	}

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

-- | An exception used to indicate that the connection was restricted.
data ConnectionRestricted = ConnectionRestricted String
	deriving (Show, Typeable)

instance Exception ConnectionRestricted

type IPAddrString = String

-- | Constructs a ConnectionRestricted, passing the function a string
-- containing the IP address.
addrConnectionRestricted :: (IPAddrString -> String) -> AddrInfo -> ConnectionRestricted
addrConnectionRestricted mkmessage = 
	ConnectionRestricted . mkmessage . showSockAddress . addrAddress

data ProxyRestricted = ProxyRestricted
	deriving (Show)

-- | Adjusts a ManagerSettings to enforce a Restriction. The restriction
-- will be checked each time a Request is made, and for each redirect
-- followed.
--
-- The http proxy is also checked against the Restriction, and if
-- access to it is blocked, the http proxy will not be used.
restrictManagerSettings
	:: Restriction
	-> ManagerSettings
	-> IO (ManagerSettings, Maybe ProxyRestricted)
restrictManagerSettings cfg base = restrictProxy cfg $ base
	{ managerRawConnection = restrictedRawConnection cfg
	, managerTlsConnection = restrictedTlsConnection cfg
	, managerWrapException = wrapOurExceptions base
	}

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
restrictedRawConnection cfg = getConnection cfg Nothing

restrictedTlsConnection :: Restriction -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
restrictedTlsConnection cfg = getConnection cfg $
	-- It's not possible to access the TLSSettings
	-- used in the base ManagerSettings. So, use the default
	-- value, which is the same thing http-client-tls defaults to.
	-- Since changing from the default settings can only make TLS
	-- less secure, this is not a big problem.
	Just def



-- Based on Network.HTTP.Client.TLS.getTlsConnection.
--
-- Checks the Restriction
--
-- Does not support SOCKS.
getConnection :: Restriction -> Maybe NC.TLSSettings -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
getConnection cfg tls = do
	context <- NC.initConnectionContext
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
