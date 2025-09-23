{- git-annex Map log
 -
 - This is used to store a Map, in a way that can be union merged.
 -
 - A line of the log will look like: "timestamp field value"
 -
 - The field names cannot contain whitespace.
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.MapLog (
	module Logs.MapLog,
	VectorClock,
	currentVectorClock,
) where

import Common
import Annex.VectorClock
import Logs.Line
import Utility.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

data LogEntry v = LogEntry
	{ changed :: VectorClock
	, value :: v
	} deriving (Eq, Show)

instance Arbitrary v => Arbitrary (LogEntry v) where
	arbitrary = LogEntry <$> arbitrary <*> arbitrary

newtype MapLog f v = MapLog (M.Map f (LogEntry v))
	deriving (Show, Eq)

instance Ord f => Semigroup (MapLog f v)
  where
	a <> MapLog b = foldl' (\m (f, v) -> addMapLog f v m) a (M.toList b)

instance Ord f => Monoid (MapLog f v)
  where
	mempty = MapLog M.empty

fromMapLog :: MapLog f v -> M.Map f (LogEntry v)
fromMapLog (MapLog m) = m

buildMapLog :: (f -> Builder) -> (v -> Builder) -> MapLog f v -> Builder
buildMapLog fieldbuilder valuebuilder (MapLog m) = 
	mconcat $ map genline $ M.toList m
  where
	genline (f, LogEntry c v) = 
		buildVectorClock c <> sp 
			<> fieldbuilder f <> sp 
			<> valuebuilder v <> nl
	sp = charUtf8 ' '
	nl = charUtf8 '\n'

parseMapLog :: Ord f => A.Parser f -> A.Parser v -> L.ByteString -> MapLog f v
parseMapLog fieldparser valueparser = 
	parseMapLogWith (mapLogParser fieldparser valueparser)

parseMapLogWith :: Ord f => A.Parser (MapLog f v) -> L.ByteString -> MapLog f v
parseMapLogWith parser = fromMaybe (MapLog M.empty) 
	. AL.maybeResult
	. AL.parse parser

mapLogParser :: Ord f => A.Parser f -> A.Parser v -> A.Parser (MapLog f v)
mapLogParser fieldparser valueparser = mapLogParser' $ do
	c <- vectorClockParser
	_ <- A8.char ' '
	w <- A8.takeTill (== ' ')
	f <- either fail return $
		A.parseOnly (fieldparser <* A.endOfInput) w
	_ <- A8.char ' '
	v <- valueparser
	A.endOfInput
	return (f, LogEntry c v)

mapLogParser' :: Ord f => A.Parser (f, LogEntry v) -> A.Parser (MapLog f v)
mapLogParser' p = MapLog . M.fromListWith best
	<$> parseLogLines p

changeMapLog :: Ord f => CandidateVectorClock -> f -> v -> MapLog f v -> MapLog f v
changeMapLog c f v (MapLog m) = MapLog (M.insert f (LogEntry c' v) m)
  where
	c' = case M.lookup f m of
		Nothing -> advanceVectorClock c []
		Just old -> advanceVectorClock c [changed old]

{- Only add an LogEntry if it's newer (or at least as new as) than any
 - existing LogEntry for a field. -}
addMapLog :: Ord f => f -> LogEntry v -> MapLog f v -> MapLog f v
addMapLog f v (MapLog m) = MapLog (M.insertWith best f v m)

filterMapLogWith :: (f -> LogEntry v -> Bool) -> MapLog f v -> MapLog f v
filterMapLogWith f (MapLog m) = MapLog (M.filterWithKey f m)

mapLogWithKey :: (f -> LogEntry v -> LogEntry v) -> MapLog f v -> MapLog f v
mapLogWithKey f (MapLog m) = MapLog (M.mapWithKey f m)

{- Converts a MapLog into a simple Map without the timestamp information.
 - This is a one-way trip, but useful for code that never needs to change
 - the log. -}
simpleMap :: MapLog f v -> M.Map f v
simpleMap (MapLog m) = M.map value m

best :: LogEntry v -> LogEntry v -> LogEntry v
best new old
	| changed old > changed new = old
	| otherwise = new

prop_addMapLog_sane :: Bool
prop_addMapLog_sane = newWins && newestWins
  where
	newWins = addMapLog "foo" (LogEntry (VectorClock 1) "new") l == l2
	newestWins = addMapLog "foo" (LogEntry (VectorClock 1) "newest") l2 /= l2

	l = MapLog (M.fromList [("foo", LogEntry (VectorClock 0) "old")])
	l2 = MapLog (M.fromList [("foo", LogEntry (VectorClock 1) "new")])
