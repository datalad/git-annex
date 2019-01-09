{- git-annex Map log
 -
 - This is used to store a Map, in a way that can be union merged.
 -
 - A line of the log will look like: "timestamp field value"
 -
 - Copyright 2014, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.MapLog (
	module Logs.MapLog,
	VectorClock,
	currentVectorClock,
) where

import Common
import Annex.VectorClock
import Logs.Line

import qualified Data.Map.Strict as M
import Data.ByteString.Builder

data LogEntry v = LogEntry
	{ changed :: VectorClock
	, value :: v
	} deriving (Eq)

type MapLog f v = M.Map f (LogEntry v)

buildMapLog :: (f -> Builder) -> (v -> Builder) -> MapLog f v -> Builder
buildMapLog fieldbuilder valuebuilder = mconcat . map genline . M.toList
  where
	genline (f, LogEntry c v) = 
		buildVectorClock c <> sp 
			<> fieldbuilder f <> sp 
			<> valuebuilder v <> nl
	sp = charUtf8 ' '
	nl = charUtf8 '\n'


parseMapLog :: Ord f => (String -> Maybe f) -> (String -> Maybe v) -> String -> MapLog f v
parseMapLog fieldparser valueparser = M.fromListWith best . mapMaybe parse . splitLines
  where
	parse line = do
		let (sc, rest) = splitword line
		    (sf, sv) = splitword rest
		c <- parseVectorClock sc
		f <- fieldparser sf
		v <- valueparser sv
		Just (f, LogEntry c v)
	splitword = separate (== ' ')

changeMapLog :: Ord f => VectorClock -> f -> v -> MapLog f v -> MapLog f v
changeMapLog c f v = M.insert f $ LogEntry c v

{- Only add an LogEntry if it's newer (or at least as new as) than any
 - existing LogEntry for a field. -}
addMapLog :: Ord f => f -> LogEntry v -> MapLog f v -> MapLog f v
addMapLog = M.insertWith best

{- Converts a MapLog into a simple Map without the timestamp information.
 - This is a one-way trip, but useful for code that never needs to change
 - the log. -}
simpleMap :: MapLog f v -> M.Map f v
simpleMap = M.map value

best :: LogEntry v -> LogEntry v -> LogEntry v
best new old
	| changed old > changed new = old
	| otherwise = new

prop_addMapLog_sane :: Bool
prop_addMapLog_sane = newWins && newestWins
  where
	newWins = addMapLog ("foo") (LogEntry (VectorClock 1) "new") l == l2
	newestWins = addMapLog ("foo") (LogEntry (VectorClock 1) "newest") l2 /= l2

	l = M.fromList [("foo", LogEntry (VectorClock 0) "old")]
	l2 = M.fromList [("foo", LogEntry (VectorClock 1) "new")]
