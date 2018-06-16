{- | Restricted Manager for http-client-tls
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 - 
 - Portions from http-client-tls Copyright (c) 2013 Michael Snoyman
 -
 - License: MIT
 -}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Utility.HttpManagerRestricted (
	restrictManagerSettings,
	Restriction(..),
	ConnectionRestricted(..),
	addrConnectionRestricted,
) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal (ManagerSettings(..), Connection)
import Network.HTTP.Client.TLS
import Network.Socket
import Network.BSD (getProtocolNumber)
import Control.Exception
import qualified Network.Connection as NC
import Data.ByteString (ByteString)
import Data.Default
import Data.Typeable

data Restriction = Restriction
	{ addressRestriction :: AddrInfo -> Maybe ConnectionRestricted
	}

data ConnectionRestricted = ConnectionRestricted String
	deriving (Show, Typeable)

instance Exception ConnectionRestricted

addrConnectionRestricted :: AddrInfo -> ConnectionRestricted
addrConnectionRestricted addr = ConnectionRestricted $ unwords
	 [ "Configuration does not allow accessing address"
	 , case addrAddress addr of
	 	a@(SockAddrInet _ _) ->
			takeWhile (/= ':') $ show a
		a@(SockAddrInet6 _ _ _ _) -> 
			takeWhile (/= ']') $ drop 1 $ show a
	 ]

-- | Adjusts a ManagerSettings to check a Restriction.
--
-- Note that connections to http proxies are not checked.
-- Use `managerSetProxy noProxy` to prevent connections through http
-- proxies.
restrictManagerSettings ::  Restriction -> ManagerSettings -> ManagerSettings
restrictManagerSettings cfg base = base
	{ managerRawConnection = restrictedRawConnection cfg
	, managerTlsConnection = restrictedTlsConnection cfg
	, managerWrapException = \req ->
		let wrapper se
			| Just (_ :: ConnectionRestricted) <- fromException se = 
		         	toException $ HttpExceptionRequest req $
					InternalException se
			| otherwise = se
		 in handle $ throwIO . wrapper
	}

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
	return $ \_ha host port -> bracketOnError
		(go context host port)
		NC.connectionClose
		convertConnection
   where
	go context host port = do
		let connparams = NC.ConnectionParams
			{ NC.connectionHostname = host
			, NC.connectionPort = fromIntegral port
			, NC.connectionUseSecure = tls
			, NC.connectionUseSocks = Nothing -- unsupprted
			}
		proto <- getProtocolNumber "tcp"
		let serv = show port
		let hints = defaultHints
			{ addrFlags = [AI_ADDRCONFIG]
			, addrProtocol = proto
			, addrSocketType = Stream
			}
		addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
		bracketOnError
			(firstSuccessful $ map tryToConnect addrs)
			close
			(\sock -> NC.connectFromSocket context sock connparams)
	  where
		tryToConnect addr = case addressRestriction cfg addr of
			Nothing -> bracketOnError
				(socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
				close
				(\sock -> connect sock (addrAddress addr) >> return sock)
			Just r -> throwIO r
		firstSuccessful [] = throwIO $ NC.HostNotResolved host
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
