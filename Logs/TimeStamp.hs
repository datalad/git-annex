{- log timestamp parsing
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Logs.TimeStamp where

import Data.Time.Clock.POSIX
import Data.Time
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif

import Common

{- Parses how POSIXTime shows itself: "1431286201.113452s"
 - Also handles the format with no fractional seconds. -}
parsePOSIXTime :: String -> Maybe POSIXTime
#if MIN_VERSION_time(1,5,0)
parsePOSIXTime s = utcTimeToPOSIXSeconds <$> parseTimeM True defaultTimeLocale "%s%Qs" s
#else
parsePOSIXTime s = utcTimeToPOSIXSeconds <$> parseTime defaultTimeLocale "%s%Qs" s
#endif

formatPOSIXTime :: String -> POSIXTime -> String
formatPOSIXTime fmt t = formatTime defaultTimeLocale fmt (posixSecondsToUTCTime t)
