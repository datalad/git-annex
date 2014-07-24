{- Chunk logs, pure operations.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Chunk.Pure where

import Common.Annex
import Logs.MapLog
import Data.Int

type ChunkSize = Int64

type ChunkCount = Integer

type ChunkLog = MapLog (UUID, ChunkSize) ChunkCount

parseLog :: String -> ChunkLog
parseLog = parseMapLog fieldparser valueparser
  where
	fieldparser s =
		let (u,sz) = separate (== ':') s
		in (,) <$> pure (toUUID u) <*> readish sz
	valueparser = readish

showLog :: ChunkLog -> String
showLog = showMapLog fieldshower valueshower
  where
	fieldshower (u, sz) = fromUUID u ++ ':' : show sz
	valueshower = show
