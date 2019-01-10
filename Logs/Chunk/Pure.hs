{- Chunk logs, pure operations.
 -
 - Copyright 2014, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Chunk.Pure 
	( ChunkMethod(..)
	, ChunkSize
	, ChunkCount
	, ChunkLog
	, parseLog
	, buildLog
	) where

import Annex.Common
import Logs.MapLog
import Data.Int

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

-- Currently chunks are all fixed size, but other chunking methods
-- may be added.
data ChunkMethod = FixedSizeChunks ChunkSize | UnknownChunks S.ByteString
	deriving (Ord, Eq, Show)

type ChunkSize = Int64

-- 0 when chunks are no longer present
type ChunkCount = Integer

type ChunkLog = MapLog (UUID, ChunkMethod) ChunkCount

buildChunkMethod :: ChunkMethod -> Builder
buildChunkMethod (FixedSizeChunks sz) = int64Dec sz
buildChunkMethod (UnknownChunks s) = byteString s

chunkMethodParser :: A.Parser ChunkMethod
chunkMethodParser =
	(FixedSizeChunks <$> A8.decimal) <|> (UnknownChunks <$> A.takeByteString)

buildLog :: ChunkLog -> Builder
buildLog = buildMapLog fieldbuilder valuebuilder
  where
	fieldbuilder (u, m) = buildUUID u <> sep <> buildChunkMethod m
	valuebuilder = integerDec
	sep = charUtf8 ':'

parseLog :: L.ByteString -> ChunkLog
parseLog = parseMapLog fieldparser valueparser
  where
	fieldparser = (,)
		<$> (toUUID <$> A8.takeTill (== ':'))
		<* A8.char ':'
		<*> chunkMethodParser
		<* A.endOfInput
	valueparser = A8.decimal
