{- git-annex maxsize log
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.MaxSize (
	MaxSize(..),
	getMaxSizes,
	recordMaxSize,
) where

import qualified Annex
import Annex.Common
import Types.RepoSize
import Logs
import Logs.UUIDBased
import Logs.MapLog
import qualified Annex.Branch

import qualified Data.Map as M
import Data.ByteString.Builder
import qualified Data.Attoparsec.ByteString as A

getMaxSizes :: Annex (M.Map UUID MaxSize)
getMaxSizes = maybe loadMaxSizes return =<< Annex.getState Annex.maxsizes

loadMaxSizes :: Annex (M.Map UUID MaxSize)
loadMaxSizes = do
	maxsizes <- M.map value . fromMapLog . parseLogNew parseMaxSize
		<$> Annex.Branch.get maxSizeLog
	Annex.changeState $ \s -> s { Annex.maxsizes = Just maxsizes }
	return maxsizes

recordMaxSize :: UUID -> MaxSize -> Annex ()
recordMaxSize uuid maxsize = do
	c <- currentVectorClock
	Annex.Branch.change (Annex.Branch.RegardingUUID [uuid]) maxSizeLog $
		(buildLogNew buildMaxSize)
			. changeLog c uuid maxsize
			. parseLogNew parseMaxSize

buildMaxSize :: MaxSize -> Builder
buildMaxSize (MaxSize n) = byteString (encodeBS (show n))

parseMaxSize :: A.Parser MaxSize
parseMaxSize = maybe (fail "maxsize parse failed") (pure . MaxSize)
	. readish . decodeBS =<< A.takeByteString
