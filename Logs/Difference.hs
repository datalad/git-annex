{- git-annex difference log
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Difference (
	recordDifferences,
	recordedDifferences,
	recordedDifferencesFor,
	module Logs.Difference.Pure
) where

import qualified Data.Map as M
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString.Builder

import Annex.Common
import Types.Difference
import qualified Annex.Branch
import Logs
import Logs.UUIDBased
import Logs.Difference.Pure

recordDifferences :: Differences -> UUID -> Annex ()
recordDifferences ds@(Differences {}) uuid = do
	c <- currentVectorClock
	Annex.Branch.change differenceLog $
		buildLogOld byteString 
			. changeLog c uuid (encodeBS $ showDifferences ds) 
			. parseLogOld A.takeByteString
recordDifferences UnknownDifferences _ = return ()

-- Map of UUIDs that have Differences recorded.
-- If a new version of git-annex has added a Difference this version
-- doesn't know about, it will contain UnknownDifferences.
recordedDifferences :: Annex (M.Map UUID Differences)
recordedDifferences = parseDifferencesLog <$> Annex.Branch.get differenceLog

recordedDifferencesFor :: UUID -> Annex Differences
recordedDifferencesFor u = fromMaybe mempty . M.lookup u <$> recordedDifferences
