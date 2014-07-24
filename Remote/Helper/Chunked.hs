{- git-annex chunked remotes
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked where

import Utility.DataUnits
import Types.Remote

import qualified Data.Map as M
import Data.Int

data ChunkConfig
	= NoChunks
	| ChunkSize Int64
	| LegacyChunkSize Int64

chunkConfig :: RemoteConfig -> ChunkConfig
chunkConfig m =
	case M.lookup "chunksize" m of
		Nothing -> case M.lookup "chunk" m of
			Nothing -> NoChunks
			Just v -> ChunkSize $ readsz v "chunk"
		Just v -> LegacyChunkSize $ readsz v "chunksize"
  where
	readsz v f = case readSize dataUnits v of
		Just size | size > 0 -> fromInteger size
		_ -> error ("bad " ++ f)
