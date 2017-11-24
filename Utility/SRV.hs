{- SRV record lookup
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

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
	return $ either (const []) use r
  where
	use = orderHosts . map tohosts
	tohosts (priority, weight, port, hostname) =
		( (fromIntegral priority, fromIntegral weight)
		, (B8.toString hostname, PortNumber $ fromIntegral port)
		)

orderHosts :: [(PriorityWeight, HostPort)] -> [HostPort]
orderHosts = map snd . sortBy (compare `on` fst)
