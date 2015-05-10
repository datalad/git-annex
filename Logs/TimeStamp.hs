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

parsePOSIXTime :: String -> Maybe POSIXTime
parsePOSIXTime s = utcTimeToPOSIXSeconds <$> parseTime defaultTimeLocale "%s%Qs" s
