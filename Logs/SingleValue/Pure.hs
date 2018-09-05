{- git-annex single-value log, pure operations
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.SingleValue.Pure where

import Annex.Common
import Logs.Line
import Annex.VectorClock

import qualified Data.Set as S

class SingleValueSerializable v where
	serialize :: v -> String
	deserialize :: String -> Maybe v

data LogEntry v = LogEntry
	{ changed :: VectorClock
	, value :: v
	} deriving (Eq, Ord)

type Log v = S.Set (LogEntry v)

showLog :: (SingleValueSerializable v) => Log v -> String
showLog = unlines . map showline . S.toList
  where
	showline (LogEntry c v) = unwords [formatVectorClock c, serialize v]

parseLog :: (Ord v, SingleValueSerializable v) => String -> Log v
parseLog = S.fromList . mapMaybe parse . splitLines
  where
	parse line = do
		let (sc, s) = splitword line
		c <- parseVectorClock sc
		v <- deserialize s
		Just (LogEntry c v)
	splitword = separate (== ' ')

newestValue :: Log v -> Maybe v
newestValue s
	| S.null s = Nothing
	| otherwise = Just (value $ S.findMax s)
