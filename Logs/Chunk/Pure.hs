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
import Data.ByteString.Builder

-- Currently chunks are all fixed size, but other chunking methods
-- may be added.
data ChunkMethod = FixedSizeChunks ChunkSize | UnknownChunks S.ByteString
	deriving (Ord, Eq, Show)

type ChunkSize = Int64

-- 0 when chunks are no longer present
type ChunkCount = Integer

type ChunkLog = MapLog (UUID, ChunkMethod) ChunkCount

parseChunkMethod :: String -> ChunkMethod
parseChunkMethod s = maybe (UnknownChunks $ encodeBS s) FixedSizeChunks (readish s)

buildChunkMethod :: ChunkMethod -> Builder
buildChunkMethod (FixedSizeChunks sz) = int64Dec sz
buildChunkMethod (UnknownChunks s) = byteString s

parseLog :: String -> ChunkLog
parseLog = parseMapLog fieldparser valueparser
  where
	fieldparser s =
		let (u,m) = separate (== ':') s
		in Just (toUUID u, parseChunkMethod m)
	valueparser = readish

buildLog :: ChunkLog -> Builder
buildLog = buildMapLog fieldbuilder valuebuilder
  where
	fieldbuilder (u, m) = byteString (fromUUID u) <> sep <> buildChunkMethod m
	valuebuilder = integerDec
	sep = charUtf8 ':'
