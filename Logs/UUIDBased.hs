{- git-annex uuid-based logs
 -
 - This is used to store information about UUIDs in a way that can
 - be union merged.
 -
 - A line of the log will look like: "UUID[ INFO[ timestamp=foo]]"
 - The timestamp is last for backwards compatability reasons,
 - and may not be present on old log lines.
 -
 - New uuid based logs instead use the form: "timestamp UUID INFO"
 - 
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.UUIDBased (
	Log,
	LogEntry(..),
	TimeStamp(..),
	parseLog,
	parseLogNew,
	parseLogWithUUID,
	showLog,
	showLogNew,
	changeLog,
	addLog,
	simpleMap,
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common
import Types.UUID
import Logs.MapLog
import Logs.TimeStamp
import Logs.Line

type Log v = MapLog UUID v

showLog :: (v -> String) -> Log v -> String
showLog shower = unlines . map showpair . M.toList
  where
	showpair (k, LogEntry (Date p) v) =
		unwords [fromUUID k, shower v, tskey ++ show p]
	showpair (k, LogEntry Unknown v) =
		unwords [fromUUID k, shower v]

parseLog :: (String -> Maybe a) -> String -> Log a
parseLog = parseLogWithUUID . const

parseLogWithUUID :: (UUID -> String -> Maybe a) -> String -> Log a
parseLogWithUUID parser = M.fromListWith best . mapMaybe parse . splitLines
  where
	parse line
		-- This is a workaround for a bug that caused
		-- NoUUID items to be stored in the log.
		-- It can be removed at any time; is just here to clean
		-- up logs where that happened temporarily.
		| " " `isPrefixOf` line = Nothing
		| null ws = Nothing
		| otherwise = parser u (unwords info) >>= makepair
	  where
		makepair v = Just (u, LogEntry ts v)
		ws = words line
		u = toUUID $ Prelude.head ws
		t = Prelude.last ws
		ts
			| tskey `isPrefixOf` t =
				pdate $ drop 1 $ dropWhile (/= '=') t
			| otherwise = Unknown
		info
			| ts == Unknown = drop 1 ws
			| otherwise = drop 1 $ beginning ws
		pdate s = case parsePOSIXTime s of
			Nothing -> Unknown
			Just d -> Date d

showLogNew :: (v -> String) -> Log v -> String
showLogNew = showMapLog fromUUID

parseLogNew :: (String -> Maybe v) -> String -> Log v
parseLogNew = parseMapLog (Just . toUUID)

changeLog :: POSIXTime -> UUID -> v -> Log v -> Log v
changeLog = changeMapLog

addLog :: UUID -> LogEntry v -> Log v -> Log v
addLog = addMapLog

tskey :: String
tskey = "timestamp="
