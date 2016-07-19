{-# LANGUAGE CPP #-}

{- git-annex Map log
 -
 - This is used to store a Map, in a way that can be union merged.
 -
 - A line of the log will look like: "timestamp field value"
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.MapLog where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common
import Logs.TimeStamp
import Logs.Line

data TimeStamp = Unknown | Date POSIXTime
	deriving (Eq, Ord, Show)

data LogEntry v = LogEntry
	{ changed :: TimeStamp
	, value :: v
	} deriving (Eq, Show)

type MapLog f v = M.Map f (LogEntry v)

showMapLog :: (f -> String) -> (v -> String) -> MapLog f v -> String
showMapLog fieldshower valueshower = unlines . map showpair . M.toList
  where
	showpair (f, LogEntry (Date p) v) =
		unwords [show p, fieldshower f, valueshower v]
	showpair (f, LogEntry Unknown v) =
		unwords ["0", fieldshower f, valueshower v]

parseMapLog :: Ord f => (String -> Maybe f) -> (String -> Maybe v) -> String -> MapLog f v
parseMapLog fieldparser valueparser = M.fromListWith best . mapMaybe parse . splitLines
  where
	parse line = do
		let (ts, rest) = splitword line
		    (sf, sv) = splitword rest
		date <- Date <$> parsePOSIXTime ts
		f <- fieldparser sf
		v <- valueparser sv
		Just (f, LogEntry date v)
	splitword = separate (== ' ')

changeMapLog :: Ord f => POSIXTime -> f -> v -> MapLog f v -> MapLog f v
changeMapLog t f v = M.insert f $ LogEntry (Date t) v

{- Only add an LogEntry if it's newer (or at least as new as) than any
 - existing LogEntry for a field. -}
addMapLog :: Ord f => f -> LogEntry v -> MapLog f v -> MapLog f v
addMapLog = M.insertWith' best

{- Converts a MapLog into a simple Map without the timestamp information.
 - This is a one-way trip, but useful for code that never needs to change
 - the log. -}
simpleMap :: MapLog f v -> M.Map f v
simpleMap = M.map value

best :: LogEntry v -> LogEntry v -> LogEntry v
best new old
	| changed old > changed new = old
	| otherwise = new

-- Unknown is oldest.
prop_TimeStamp_sane :: Bool
prop_TimeStamp_sane = Unknown < Date 1

prop_addMapLog_sane :: Bool
prop_addMapLog_sane = newWins && newestWins
  where
	newWins = addMapLog ("foo") (LogEntry (Date 1) "new") l == l2
	newestWins = addMapLog ("foo") (LogEntry (Date 1) "newest") l2 /= l2

	l = M.fromList [("foo", LogEntry (Date 0) "old")]
	l2 = M.fromList [("foo", LogEntry (Date 1) "new")]
