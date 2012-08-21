{- DBus utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utility.DBus where

import DBus.Client
import DBus
import Data.Maybe

type ServiceName = String

listServiceNames :: Client -> IO [ServiceName]
listServiceNames client = do
	reply <- callDBus client "ListNames" []
	return $ fromMaybe [] $ fromVariant (methodReturnBody reply !! 0)

callDBus :: Client -> MemberName -> [Variant] -> IO MethodReturn
callDBus client name params = call_ client $
	(methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" name)
		{ methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallBody = params
		}
