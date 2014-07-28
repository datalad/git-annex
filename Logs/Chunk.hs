{- Chunk logs.
 -
 - An object can be stored in chunked for on a remote; these logs keep
 - track of the chunk size used, and the number of chunks.
 -
 - It's possible for a single object to be stored multiple times on the
 - same remote using different chunk sizes. So, while this is a MapLog, it
 - is not a normal UUIDBased log. Intead, it's a map from UUID and chunk
 - size to number of chunks.
 -
 - Format: "timestamp uuid:chunksize chunkcount"
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Chunk (
	ChunkMethod(..),
	ChunkSize,
	ChunkCount,
	chunksStored,
	chunksRemoved,
	getCurrentChunks,
) where

import Common.Annex
import Logs
import Logs.MapLog
import qualified Annex.Branch
import Logs.Chunk.Pure

import qualified Data.Map as M
import Data.Time.Clock.POSIX

chunksStored :: UUID -> Key -> ChunkMethod -> ChunkCount -> Annex ()
chunksStored u k chunkmethod chunkcount = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change (chunkLogFile k) $
		showLog . changeMapLog ts (u, chunkmethod) chunkcount . parseLog

chunksRemoved :: UUID -> Key -> ChunkMethod -> Annex ()
chunksRemoved u k chunkmethod = chunksStored u k chunkmethod 0

getCurrentChunks :: UUID -> Key -> Annex [(ChunkMethod, ChunkCount)]
getCurrentChunks u k = select . parseLog <$> Annex.Branch.get (chunkLogFile k)
  where
	select = filter (\(_m, ct) -> ct > 0)
		. map (\((_ku, m), l) -> (m, value l))
		. M.toList
		. M.filterWithKey (\(ku, _m) _ -> ku == u)
