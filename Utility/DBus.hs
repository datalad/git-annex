{- DBus utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Utility.DBus where

import Utility.PartialPrelude
import Utility.Exception

import DBus.Client
import DBus
import Data.Maybe
import Control.Concurrent
import Control.Exception as E

type ServiceName = String

listServiceNames :: Client -> IO [ServiceName]
listServiceNames client = do
	reply <- callDBus client "ListNames" []
	return $ fromMaybe [] $ fromVariant =<< headMaybe (methodReturnBody reply)

callDBus :: Client -> MemberName -> [Variant] -> IO MethodReturn
callDBus client name params = call_ client $
	(methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" name)
		{ methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallBody = params
		}

{- Connects to the bus, and runs the client action.
 - 
 - Throws a ClientError, and closes the connection if it fails to
 - process an incoming message, or if the connection is lost.
 - Unlike DBus's usual interface, this error is thrown at the top level,
 - rather than inside the clientThreadRunner, so it can be caught, and
 - runClient re-run as needed. -}
runClient :: IO (Maybe Address) -> (Client -> IO ()) -> IO ()
runClient getaddr clientaction = do
	env <- getaddr
	case env of
		Nothing -> throwIO (clientError "runClient: unable to determine DBUS address")
		Just addr -> do
			{- The clientaction will set up listeners, which
			 - run in a different thread. We block while
			 - they're running, until our threadrunner catches
			 - a ClientError, which it will put into the MVar
			 - to be rethrown here. -}
			mv <- newEmptyMVar
			let tr = threadrunner (putMVar mv)
			let opts = defaultClientOptions { clientThreadRunner = tr }
			client <- connectWith opts addr
			clientaction client
			e <- takeMVar mv
			disconnect client
			throw e
  where
	threadrunner storeerr io = loop
	  where
		loop = catchClientError (io >> loop) storeerr

{- Connects to the bus, and runs the client action.
 -
 - If the connection is lost, runs onretry, which can do something like
 - a delay, or printing a warning, and has a state value (useful for
 - exponential backoff). Once onretry returns, the connection is retried.
 -}
persistentClient :: IO (Maybe Address) -> v -> (SomeException -> v -> IO v) -> (Client -> IO ()) -> IO ()
persistentClient getaddr v onretry clientaction =
	{- runClient can fail with not just ClientError, but also other
	 - things, if dbus is not running. Let async exceptions through. -}
	runClient getaddr clientaction `catchNonAsync` retry
  where
	retry e = do
		v' <- onretry e v
		persistentClient getaddr v' onretry clientaction

{- Catches only ClientError -}
catchClientError :: IO () -> (ClientError -> IO ()) -> IO ()
catchClientError io handler =
	either handler return =<< (E.try io :: IO (Either ClientError ()))
