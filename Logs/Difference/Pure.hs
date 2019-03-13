{- git-annex difference log, pure functions
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Difference.Pure (
	allDifferences,
	parseDifferencesLog,
) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A

import Annex.Common
import Types.Difference
import Logs.UUIDBased

parseDifferencesLog :: L.ByteString -> (M.Map UUID Differences)
parseDifferencesLog = simpleMap
	. parseLogOld (readDifferences . decodeBS <$> A.takeByteString)

-- The sum of all recorded differences, across all UUIDs.
allDifferences :: M.Map UUID Differences -> Differences
allDifferences = mconcat . M.elems
