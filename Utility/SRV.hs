{- SRV record lookup
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.SRV (
	mkSRVTcp,
	mkSRV,
	lookupSRV,
	HostPort,
) where

import Data.Function
import Data.List
import Network
import qualified Network.DNS.Lookup as DNS
import Network.DNS.Resolver
import qualified Data.ByteString.UTF8 as B8

newtype SRV = SRV String
	deriving (Show, Eq)

type HostPort = (HostName, PortID)

type PriorityWeight = (Int, Int) -- sort by priority first, then weight

mkSRV :: String -> String -> HostName -> SRV
mkSRV transport protocol host = SRV $ concat
	["_", protocol, "._", transport, ".", host]

mkSRVTcp :: String -> HostName -> SRV
mkSRVTcp = mkSRV "tcp"

{- Returns an ordered list, with highest priority hosts first.
 -
 - On error, returns an empty list. -}
lookupSRV :: SRV -> IO [HostPort]
lookupSRV (SRV srv) = do
	seed <- makeResolvSeed defaultResolvConf
	r <- withResolver seed $ flip DNS.lookupSRV $ B8.fromString srv
	return $
#if MIN_VERSION_dns(1,0,0)
		either (const []) use r
#else
		maybe [] use r
#endif
  where
	use = orderHosts . map tohosts
	tohosts (priority, weight, port, hostname) =
		( (priority, weight)
		, (B8.toString hostname, PortNumber $ fromIntegral port)
		)

orderHosts :: [(PriorityWeight, HostPort)] -> [HostPort]
orderHosts = map snd . sortBy (compare `on` fst)
