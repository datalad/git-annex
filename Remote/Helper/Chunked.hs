{- git-annex chunked remotes
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked
	( ChunkSize
	, ChunkConfig(..)
	, chunkConfig
	, meteredWriteFileChunks
	) where

import Common.Annex
import Utility.DataUnits
import Types.Remote
import Logs.Chunk.Pure (ChunkSize)
import Utility.Metered

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

data ChunkConfig
	= NoChunks
	| UnpaddedChunks ChunkSize
	| LegacyChunks ChunkSize

chunkConfig :: RemoteConfig -> ChunkConfig
chunkConfig m =
	case M.lookup "chunksize" m of
		Nothing -> case M.lookup "chunk" m of
			Nothing -> NoChunks
			Just v -> UnpaddedChunks $ readsz v "chunk"
		Just v -> LegacyChunks $ readsz v "chunksize"
  where
	readsz v f = case readSize dataUnits v of
		Just size | size > 0 -> fromInteger size
		_ -> error ("bad " ++ f)

{- Writes a series of chunks to a file. The feeder is called to get
 - each chunk. -}
meteredWriteFileChunks :: MeterUpdate -> FilePath -> [v] -> (v -> IO L.ByteString) -> IO ()
meteredWriteFileChunks meterupdate dest chunks feeder =
	withBinaryFile dest WriteMode $ \h ->
		forM_ chunks $
			meteredWrite meterupdate h <=< feeder
