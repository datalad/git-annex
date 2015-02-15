{- Network.URI
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.URI where

-- Old versions of network lacked an Ord for URI
#if ! MIN_VERSION_network(2,4,0)
import Network.URI

instance Ord URI where
	a `compare` b = show a `compare` show b
#endif
