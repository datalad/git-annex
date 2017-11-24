{- log timestamp parsing
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Logs.TimeStamp where

import Utility.PartialPrelude
import Utility.Misc

import Data.Time.Clock.POSIX
import Data.Time
import Data.Ratio
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif

{- Parses how POSIXTime shows itself: "1431286201.113452s"
 - Also handles the format with no fractional seconds. -}
parsePOSIXTime :: String -> Maybe POSIXTime
parsePOSIXTime s = do
	let (sn, sd) = separate (== '.') s
	n <- readi sn
	if null sd 
		then return (fromIntegral n)
		else do
			d <- readi sd
			let r = d % (10 ^ (length sd - 1))
			return (fromIntegral n + fromRational r)
  where
	readi :: String -> Maybe Integer
	readi = readish

formatPOSIXTime :: String -> POSIXTime -> String
formatPOSIXTime fmt t = formatTime defaultTimeLocale fmt (posixSecondsToUTCTime t)
