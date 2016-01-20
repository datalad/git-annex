{- git-annex difference log
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Difference (
	recordDifferences,
	recordedDifferences,
	recordedDifferencesFor,
	module Logs.Difference.Pure
) where

import Data.Time.Clock.POSIX
import qualified Data.Map as M

import Annex.Common
import Types.Difference
import qualified Annex.Branch
import Logs
import Logs.UUIDBased
import Logs.Difference.Pure

recordDifferences :: Differences -> UUID -> Annex ()
recordDifferences ds@(Differences {}) uuid = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change differenceLog $
		showLog id . changeLog ts uuid (showDifferences ds) . parseLog Just
recordDifferences UnknownDifferences _ = return ()

-- Map of UUIDs that have Differences recorded.
-- If a new version of git-annex has added a Difference this version
-- doesn't know about, it will contain UnknownDifferences.
recordedDifferences :: Annex (M.Map UUID Differences)
recordedDifferences = parseDifferencesLog <$> Annex.Branch.get differenceLog

recordedDifferencesFor :: UUID -> Annex Differences
recordedDifferencesFor u = fromMaybe mempty . M.lookup u 
	<$> recordedDifferences
