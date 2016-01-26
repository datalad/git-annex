{- SRV record lookup
 -
 - Uses either the the standalone Haskell DNS package, or the host command.
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
	lookupSRVHost,
	HostPort,
) where

import Utility.Process
import Utility.Exception
import Utility.PartialPrelude

import Network
import Data.Function
import Data.List
import Data.Maybe
import Control.Applicative
import Prelude

#ifdef WITH_DNS
import qualified Network.DNS.Lookup as DNS
import Network.DNS.Resolver
import qualified Data.ByteString.UTF8 as B8
#endif

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
#ifdef WITH_DNS
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
#else
lookupSRV = lookupSRVHost
#endif

lookupSRVHost :: SRV -> IO [HostPort]
lookupSRVHost (SRV srv) = catchDefaultIO [] $ 
	parseSrvHost <$> readProcessEnv "host" ["-t", "SRV", "--", srv]
		-- clear environment, to avoid LANG affecting output
		(Just [])

parseSrvHost :: String -> [HostPort]
parseSrvHost = orderHosts . catMaybes . map parse . lines
  where
	parse l = case words l of
		[_, _, _, _, spriority, sweight, sport, hostname] -> do
			let v = 
				( readish sport :: Maybe Int
				, readish spriority :: Maybe Int
				, readish sweight :: Maybe Int
				)
			case v of
				(Just port, Just priority, Just weight) -> Just
					( (priority, weight)
					, (hostname, PortNumber $ fromIntegral port)
					)
				_ -> Nothing
		_ -> Nothing

orderHosts :: [(PriorityWeight, HostPort)] -> [HostPort]
orderHosts = map snd . sortBy (compare `on` fst)
