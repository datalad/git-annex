{- git-annex single-value log
 -
 - This is used to store a value in a way that can be union merged.
 -
 - A line of the log will look like: "timestamp value"
 -
 - The line with the newest timestamp wins.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.SingleValue where

import Annex.Common
import qualified Annex.Branch
import Logs.TimeStamp
import Logs.Line

import qualified Data.Set as S
import Data.Time.Clock.POSIX

class SingleValueSerializable v where
	serialize :: v -> String
	deserialize :: String -> Maybe v

data LogEntry v = LogEntry
	{ changed :: POSIXTime
	, value :: v
	} deriving (Eq, Show, Ord)

type Log v = S.Set (LogEntry v)

showLog :: (SingleValueSerializable v) => Log v -> String
showLog = unlines . map showline . S.toList
  where
	showline (LogEntry t v) = unwords [show t, serialize v]

parseLog :: (Ord v, SingleValueSerializable v) => String -> Log v
parseLog = S.fromList . mapMaybe parse . splitLines
  where
	parse line = do
		let (ts, s) = splitword line
		date <- parsePOSIXTime ts
		v <- deserialize s
		Just (LogEntry date v)
	splitword = separate (== ' ')

newestValue :: Log v -> Maybe v
newestValue s
	| S.null s = Nothing
	| otherwise = Just (value $ S.findMax s)

readLog :: (Ord v, SingleValueSerializable v) => FilePath -> Annex (Log v)
readLog = parseLog <$$> Annex.Branch.get

getLog :: (Ord v, SingleValueSerializable v) => FilePath -> Annex (Maybe v)
getLog = newestValue <$$> readLog

setLog :: (SingleValueSerializable v) => FilePath -> v -> Annex ()
setLog f v = do
	now <- liftIO getPOSIXTime
	let ent = LogEntry now v
	Annex.Branch.change f $ \_old -> showLog (S.singleton ent)
