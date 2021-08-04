{- git-annex single-value log, pure operations
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.SingleValue.Pure where

import Annex.Common
import Logs.Line
import Annex.VectorClock

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString.Builder

class SingleValueSerializable v where
	serialize :: v -> B.ByteString
	deserialize :: B.ByteString -> Maybe v

data LogEntry v = LogEntry
	{ changed :: VectorClock
	, value :: v
	} deriving (Eq, Ord)

type Log v = S.Set (LogEntry v)

buildLog :: (SingleValueSerializable v) => Log v -> Builder
buildLog = mconcat . map genline . S.toList
  where
	genline (LogEntry c v) =
		buildVectorClock c
			<> sp 
			<> byteString (serialize v)
			<> nl
	sp = charUtf8 ' '
	nl = charUtf8 '\n'

parseLog :: (Ord v, SingleValueSerializable v) => L.ByteString -> Log v
parseLog = S.fromList . parseLog'

parseLog' :: SingleValueSerializable v => L.ByteString -> [LogEntry v]
parseLog' = fromMaybe [] . A.maybeResult . A.parse (logParser <* A.endOfInput)

logParser :: SingleValueSerializable v => A.Parser [LogEntry v]
logParser = parseLogLines $ LogEntry
	<$> vectorClockParser
	<* char ' '
	<*> (parsevalue =<< A.takeByteString)
  where
	parsevalue = maybe (fail "log line parse failure") return . deserialize

newestValue :: Log v -> Maybe v
newestValue s
	| S.null s = Nothing
	| otherwise = Just (value $ S.findMax s)
