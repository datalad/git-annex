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
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.UUIDBased (
	Log,
	LogEntry(..),
	VectorClock,
	currentVectorClock,
	parseLog,
	parseLogNew,
	parseLogWithUUID,
	buildLog,
	buildLogNew,
	changeLog,
	addLog,
	simpleMap,
) where

import qualified Data.Map as M

import Common
import Types.UUID
import Annex.VectorClock
import Logs.MapLog
import Logs.Line

import Data.ByteString.Builder

type Log v = MapLog UUID v

buildLog :: (v -> Builder) -> Log v -> Builder
buildLog builder = mconcat . map genline . M.toList
  where
	genline (u, LogEntry c@(VectorClock {}) v) =
		buildUUID u <> sp <> builder v <> sp <>
			byteString "timestamp=" <> buildVectorClock c <> nl
	genline (u, LogEntry Unknown v) =
		buildUUID u <> sp <> builder v <> nl
	sp = charUtf8 ' '
	nl = charUtf8 '\n'

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
			| tskey `isPrefixOf` t = fromMaybe Unknown $
				parseVectorClock $ drop 1 $ dropWhile (/= '=') t
			| otherwise = Unknown
		info
			| ts == Unknown = drop 1 ws
			| otherwise = drop 1 $ beginning ws

buildLogNew :: (v -> Builder) -> Log v -> Builder
buildLogNew = buildMapLog buildUUID

parseLogNew :: (String -> Maybe v) -> String -> Log v
parseLogNew = parseMapLog (Just . toUUID)

changeLog :: VectorClock -> UUID -> v -> Log v -> Log v
changeLog = changeMapLog

addLog :: UUID -> LogEntry v -> Log v -> Log v
addLog = addMapLog

tskey :: String
tskey = "timestamp="
