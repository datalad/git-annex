{- Network.URI
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.URI where

-- Old versions of network lacked an Ord for URI
#if ! MIN_VERSION_network(2,4,0)
import Network.URI

instance Ord URI where
	a `compare` b = show a `compare` show b
#endif
