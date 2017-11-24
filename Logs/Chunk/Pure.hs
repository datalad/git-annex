{- Chunk logs, pure operations.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Chunk.Pure 
	( ChunkMethod(..)
	, ChunkSize
	, ChunkCount
	, ChunkLog
	, parseLog
	, showLog
	) where

import Annex.Common
import Logs.MapLog
import Data.Int

-- Currently chunks are all fixed size, but other chunking methods
-- may be added.
data ChunkMethod = FixedSizeChunks ChunkSize | UnknownChunks String
	deriving (Ord, Eq, Show)

type ChunkSize = Int64

-- 0 when chunks are no longer present
type ChunkCount = Integer

type ChunkLog = MapLog (UUID, ChunkMethod) ChunkCount

parseChunkMethod :: String -> ChunkMethod
parseChunkMethod s = maybe (UnknownChunks s) FixedSizeChunks (readish s)

showChunkMethod :: ChunkMethod -> String
showChunkMethod (FixedSizeChunks sz) = show sz
showChunkMethod (UnknownChunks s) = s

parseLog :: String -> ChunkLog
parseLog = parseMapLog fieldparser valueparser
  where
	fieldparser s =
		let (u,m) = separate (== sep) s
		in Just (toUUID u, parseChunkMethod m)
	valueparser = readish

showLog :: ChunkLog -> String
showLog = showMapLog fieldshower valueshower
  where
	fieldshower (u, m) = fromUUID u ++ sep : showChunkMethod m
	valueshower = show

sep :: Char
sep = ':'
