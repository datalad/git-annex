{- SRV record lookup
 -
 - Uses either the ADNS Haskell library, or if it's not installed,
 - the host command.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.SRV (
	mkSRVTcp,
	mkSRV,
	lookupSRV,
) where

import qualified Build.SysConfig
import Utility.Process
import Utility.Exception
import Utility.PartialPrelude

import Network
import Data.Function
import Data.List
import Control.Applicative
import Data.Maybe

#ifdef WITH_ADNS
import ADNS.Resolver
import Data.Either
#endif

newtype SRV = SRV String
	deriving (Show, Eq)

type HostPort = (HostName, PortID)

mkSRV :: String -> String -> HostName -> SRV
mkSRV transport protocol host = SRV $ concat
	["_", protocol, "._", transport, ".", host]

mkSRVTcp :: String -> HostName -> SRV
mkSRVTcp = mkSRV "tcp"

{- Returns an ordered list, with highest priority hosts first.
 -
 - On error, returns an empty list. -}
lookupSRV :: SRV -> IO [HostPort]
#ifdef WITH_ADNS
lookupSRV srv = initResolver [] $ \resolver -> do
	r <- catchDefaultIO (Right []) $
		resolveSRV resolver srv
	return $ either (\_ -> []) id r
#else
lookupSRV = lookupSRVHost
#endif

lookupSRVHost :: SRV -> IO [HostPort]
lookupSRVHost (SRV srv)
	| Build.SysConfig.host = catchDefaultIO [] $ 
		parseSrvHost <$> readProcessEnv "host" ["-t", "SRV", "--", srv]
			-- clear environment, to avoid LANG affecting output
			(Just [])
	| otherwise = return []

parseSrvHost :: String -> [HostPort]
parseSrvHost = map snd . reverse . sortBy cost . catMaybes . map parse . lines
	where
		cost = compare `on` fst
		parse l = case words l of
			[_, _, _, _, priority, weight, sport, hostname] -> do
				let v = readish sport :: Maybe Int
				case v of
					Nothing -> Nothing
					Just port -> Just
						( (priority, weight)
						, (hostname, PortNumber $ fromIntegral port)
						)
			_ -> Nothing

