{- git-annex uuid-based logs
 -
 - This is used to store information about UUIDs in a way that can
 - be union merged.
 -
 - The old format looks like: "UUID[ INFO[ timestamp=foo]]"
 - The timestamp is last for backwards compatibility reasons,
 - and may not be present on very old log lines.
 -
 - New uuid based logs instead use the form: "timestamp UUID INFO"
 - 
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Logs.UUIDBased (
	Log,
	LogEntry(..),
	VectorClock,
	currentVectorClock,
	parseLogOld,
	parseLogNew,
	parseLogOldWithUUID,
	buildLogOld,
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

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder
import qualified Data.DList as D

type Log v = MapLog UUID v

buildLogOld :: (v -> Builder) -> Log v -> Builder
buildLogOld builder = mconcat . map genline . M.toList
  where
	genline (u, LogEntry c@(VectorClock {}) v) =
		buildUUID u <> sp <> builder v <> sp
			<> byteString "timestamp="
			<> buildVectorClock c
			<> nl
	genline (u, LogEntry Unknown v) =
		buildUUID u <> sp <> builder v <> nl
	sp = charUtf8 ' '
	nl = charUtf8 '\n'

parseLogOld :: A.Parser a -> L.ByteString -> Log a
parseLogOld = parseLogOldWithUUID . const

parseLogOldWithUUID :: (UUID -> A.Parser a) -> L.ByteString -> Log a
parseLogOldWithUUID parser = fromMaybe M.empty . AL.maybeResult
	. AL.parse (logParserOld parser)

logParserOld :: (UUID -> A.Parser a) -> A.Parser (Log a)
logParserOld parser = M.fromListWith best <$> parseLogLines go
  where
	go = do
		u <- toUUID <$> A8.takeWhile1 (/= ' ')
		(dl, ts) <- accumval D.empty
		v <- either fail return $ A.parseOnly (parser u <* A.endOfInput)
			(S.intercalate " " $ D.toList dl)
		return (u, LogEntry ts v)
	accumval dl =
		((dl,) <$> parsetimestamp)
		<|> (A8.char ' ' *> (A8.takeWhile (/= ' ')) >>= accumval . D.snoc dl)
	parsetimestamp = 
		(A8.string " timestamp=" *> vectorClockParser <* A.endOfInput)
		<|> (const Unknown <$> A.endOfInput)

buildLogNew :: (v -> Builder) -> Log v -> Builder
buildLogNew = buildMapLog buildUUID

parseLogNew :: A.Parser v -> L.ByteString -> Log v
parseLogNew = parseMapLog (toUUID <$> A.takeByteString)

changeLog :: CandidateVectorClock -> UUID -> v -> Log v -> Log v
changeLog = changeMapLog

addLog :: UUID -> LogEntry v -> Log v -> Log v
addLog = addMapLog
