{- git-annex export log
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Export where

import qualified Data.Map as M

import Annex.Common
import qualified Annex.Branch
import qualified Git
import Logs
import Logs.UUIDBased
import Annex.UUID

-- | Get the treeish that was exported to a special remote.
--
-- If the list contains multiple items, there was an export conflict,
-- and different trees were exported to the same special remote.
getExport :: UUID -> Annex [Git.Ref]
getExport remoteuuid = nub . mapMaybe get . M.elems . simpleMap 
	. parseLogNew parseExportLog
	<$> Annex.Branch.get exportLog
  where
	get (ExportLog t u)
		| u == remoteuuid = Just t
		| otherwise = Nothing

data ExportChange = ExportChange
	{ oldTreeish :: [Git.Ref]
	, newTreeish :: Git.Ref
	}

-- | Record a change in what's exported to a special remote.
--
-- Any entries in the log for the oldTreeish will be updated to the
-- newTreeish. This way, when multiple repositories are exporting to
-- the same special remote, there's no conflict as long as they move
-- forward in lock-step.
recordExport :: UUID -> ExportChange -> Annex ()
recordExport remoteuuid ec = do
	c <- liftIO currentVectorClock
	u <- getUUID
	let val = ExportLog (newTreeish ec) remoteuuid
	Annex.Branch.change exportLog $
		showLogNew formatExportLog 
			. changeLog c u val 
			. M.mapWithKey (updateothers c u)
			. parseLogNew parseExportLog
  where
	updateothers c u theiru le@(LogEntry _ (ExportLog t remoteuuid'))
		| u == theiru || remoteuuid' /= remoteuuid || t `notElem` oldTreeish ec = le
		| otherwise = LogEntry c (ExportLog (newTreeish ec) theiru)

data ExportLog = ExportLog Git.Ref UUID

formatExportLog :: ExportLog -> String
formatExportLog (ExportLog treeish remoteuuid) =
	Git.fromRef treeish ++ " " ++ fromUUID remoteuuid

parseExportLog :: String -> Maybe ExportLog
parseExportLog s = case words s of
	(t:u:[]) -> Just $ ExportLog (Git.Ref t) (toUUID u)
	_ -> Nothing
