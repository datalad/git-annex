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
import qualified Git.Branch
import Git.Tree
import Git.FilePath
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
--
-- Also, the newTreeish is grafted into the git-annex branch. This is done
-- to ensure that it's available later.
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
	graftTreeish (newTreeish ec)
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

-- To prevent git-annex branch merge conflicts, the treeish is
-- first grafted in and then removed in a subsequent commit.
graftTreeish :: Git.Ref -> Annex ()
graftTreeish treeish = do
	branchref <- Annex.Branch.getBranch
	Tree t <- inRepo $ getTree branchref
	t' <- inRepo $ recordTree $ Tree $
		RecordedSubTree (asTopFilePath graftpoint) treeish [] : t
	commit <- inRepo $ Git.Branch.commitTree Git.Branch.AutomaticCommit
		"export tree" [branchref] t'
	origtree <- inRepo $ recordTree (Tree t)
	commit' <- inRepo $ Git.Branch.commitTree Git.Branch.AutomaticCommit
		"export tree cleanup" [commit] origtree
	inRepo $ Git.Branch.update' Annex.Branch.fullname commit'
  where
	graftpoint = "export.tree"
