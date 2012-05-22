{- git-annex uuid-based logs
 -
 - This is used to store information about a UUID in a way that can
 - be union merged.
 -
 - A line of the log will look like: "UUID[ INFO[ timestamp=foo]]"
 - The timestamp is last for backwards compatability reasons,
 - and may not be present on old log lines.
 - 
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.UUIDBased (
	Log,
	LogEntry(..),
	TimeStamp(..),
	parseLog,
	showLog,
	changeLog,
	addLog,
	simpleMap,

	prop_TimeStamp_sane,
	prop_addLog_sane,
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale

import Common
import Types.UUID

data TimeStamp = Unknown | Date POSIXTime
	deriving (Eq, Ord, Show)

data LogEntry a = LogEntry
	{ changed :: TimeStamp
	, value :: a
	} deriving (Eq, Show)

type Log a = M.Map UUID (LogEntry a)

tskey :: String
tskey = "timestamp="

showLog :: (a -> String) -> Log a -> String
showLog shower = unlines . map showpair . M.toList
	where
		showpair (k, LogEntry (Date p) v) =
			unwords [fromUUID k, shower v, tskey ++ show p]
		showpair (k, LogEntry Unknown v) =
			unwords [fromUUID k, shower v]

parseLog :: (String -> Maybe a) -> String -> Log a
parseLog parser = M.fromListWith best . mapMaybe parse . lines
	where
		parse line
			| null ws = Nothing
			| otherwise = parser (unwords info) >>= makepair
			where
				makepair v = Just (toUUID u, LogEntry ts v)
				ws = words line
				u = Prelude.head ws
				t = Prelude.last ws
				ts
					| tskey `isPrefixOf` t =
						pdate $ drop 1 $ dropWhile (/= '=') t
					| otherwise = Unknown
				info
					| ts == Unknown = drop 1 ws
					| otherwise = drop 1 $ beginning ws
				pdate s = case parseTime defaultTimeLocale "%s%Qs" s of
					Nothing -> Unknown
					Just d -> Date $ utcTimeToPOSIXSeconds d

changeLog :: POSIXTime -> UUID -> a -> Log a -> Log a
changeLog t u v = M.insert u $ LogEntry (Date t) v

{- Only add an LogEntry if it's newer (or at least as new as) than any
 - existing LogEntry for a UUID. -}
addLog :: UUID -> LogEntry a -> Log a -> Log a
addLog = M.insertWith' best

{- Converts a Log into a simple Map without the timestamp information.
 - This is a one-way trip, but useful for code that never needs to change
 - the log. -}
simpleMap :: Log a -> M.Map UUID a
simpleMap = M.map value

best :: LogEntry a -> LogEntry a -> LogEntry a
best new old
	| changed old > changed new = old
	| otherwise = new

-- Unknown is oldest.
prop_TimeStamp_sane :: Bool
prop_TimeStamp_sane = Unknown < Date 1

prop_addLog_sane :: Bool
prop_addLog_sane = newWins && newestWins
	where
		newWins = addLog (UUID "foo") (LogEntry (Date 1) "new") l == l2
		newestWins = addLog (UUID "foo") (LogEntry (Date 1) "newest") l2 /= l2

		l = M.fromList [(UUID "foo", LogEntry (Date 0) "old")]
		l2 = M.fromList [(UUID "foo", LogEntry (Date 1) "new")]
