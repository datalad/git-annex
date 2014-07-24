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

module Logs.Chunk where

import Common.Annex
import Logs
import Logs.MapLog
import qualified Annex.Branch
import Logs.Chunk.Pure

import qualified Data.Map as M
import Data.Time.Clock.POSIX

chunksStored :: UUID -> Key -> ChunkSize -> ChunkCount -> Annex ()
chunksStored u k chunksize chunkcount = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change (chunkLogFile k) $
		showLog . changeMapLog ts (u, chunksize) chunkcount . parseLog

chunksRemoved :: UUID -> Key -> ChunkSize -> Annex ()
chunksRemoved u k chunksize = chunksStored u k chunksize 0

getCurrentChunks :: UUID -> Key -> Annex [(ChunkSize, ChunkCount)]
getCurrentChunks u k = select . parseLog <$> Annex.Branch.get (chunkLogFile k)
  where
	select = filter (\(_sz, ct) -> ct > 0)
		. map (\((_ku, sz), l) -> (sz, value l))
		. M.toList
		. M.filterWithKey (\(ku, _sz) _ -> ku == u)
