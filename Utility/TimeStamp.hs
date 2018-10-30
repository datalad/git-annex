{- timestamp parsing and formatting
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.TimeStamp where

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
parsePOSIXTime = uncurry parsePOSIXTime' . separate (== '.')

{- Parses the integral and decimal part of a POSIXTime -}
parsePOSIXTime' :: String -> String -> Maybe POSIXTime
parsePOSIXTime' sn sd = do
	n <- fromIntegral <$> readi sn
	let sd' = takeWhile (/= 's') sd
	if null sd'
		then return n
		else do
			d <- readi sd'
			let r = d % (10 ^ (length sd'))
			return $ if n < 0
				then n - fromRational r
				else n + fromRational r
  where
	readi :: String -> Maybe Integer
	readi = readish

formatPOSIXTime :: String -> POSIXTime -> String
formatPOSIXTime fmt t = formatTime defaultTimeLocale fmt (posixSecondsToUTCTime t)
