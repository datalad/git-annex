{- Time for humans.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.HumanTime where

import Utility.PartialPrelude

import Data.Time.Clock.POSIX (POSIXTime)

{- Parses a human-input time duration, of the form "5h" or "1m". -}
parseDuration :: String -> Maybe POSIXTime
parseDuration s = do
	num <- readish s :: Maybe Integer
	units <- findUnits =<< lastMaybe s
	return $ fromIntegral num * units
  where
	findUnits 's' = Just 1
	findUnits 'm' = Just 60
	findUnits 'h' = Just $ 60 * 60
	findUnits 'd' = Just $ 60 * 60 * 24
	findUnits 'y' = Just $ 60 * 60 * 24 * 365
	findUnits _ = Nothing
