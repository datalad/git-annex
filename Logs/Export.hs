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
import Git.Sha
import Git.FilePath
import Logs
import Logs.UUIDBased
import Annex.UUID

data Exported = Exported
	{ exportedTreeish :: Git.Ref
	, incompleteExportedTreeish :: [Git.Ref]
	}
	deriving (Eq)

-- | Get what's been exported to a special remote.
--
-- If the list contains multiple items, there was an export conflict,
-- and different trees were exported to the same special remote.
getExport :: UUID -> Annex [Exported]
getExport remoteuuid = nub . mapMaybe get . M.elems . simpleMap 
	. parseLogNew parseExportLog
	<$> Annex.Branch.get exportLog
  where
	get (ExportLog exported u)
		| u == remoteuuid = Just exported
		| otherwise = Nothing

data ExportChange = ExportChange
	{ oldTreeish :: [Git.Ref]
	, newTreeish :: Git.Ref
	}

-- | Record a change in what's exported to a special remote.
--
-- This is called before an export begins uploading new files to the
-- remote, but after it's cleaned up any files that need to be deleted
-- from the old treeish.
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
	let val = ExportLog (Exported (newTreeish ec) []) remoteuuid
	Annex.Branch.change exportLog $
		showLogNew formatExportLog 
			. changeLog c u val 
			. M.mapWithKey (updateothers c u)
			. parseLogNew parseExportLog
  where
	updateothers c u theiru le@(LogEntry _ (ExportLog exported@(Exported { exportedTreeish = t }) remoteuuid'))
		| u == theiru || remoteuuid' /= remoteuuid || t `notElem` oldTreeish ec = le
		| otherwise = LogEntry c (ExportLog (exported { exportedTreeish = newTreeish ec }) theiru)

-- | Record the beginning of an export, to allow cleaning up from
-- interrupted exports.
--
-- This is called before any changes are made to the remote.
recordExportBeginning :: UUID -> Git.Ref -> Annex ()
recordExportBeginning remoteuuid newtree = do
	c <- liftIO currentVectorClock
	u <- getUUID
	ExportLog old _ <- fromMaybe (ExportLog (Exported emptyTree []) remoteuuid)
		. M.lookup u . simpleMap 
		. parseLogNew parseExportLog
		<$> Annex.Branch.get exportLog
	let new = old { incompleteExportedTreeish = newtree:incompleteExportedTreeish old }
	Annex.Branch.change exportLog $
		showLogNew formatExportLog 
			. changeLog c u (ExportLog new remoteuuid)
			. parseLogNew parseExportLog
	graftTreeish newtree

data ExportLog = ExportLog Exported UUID

formatExportLog :: ExportLog -> String
formatExportLog (ExportLog exported remoteuuid) = unwords $
	[ Git.fromRef (exportedTreeish exported)
	, fromUUID remoteuuid
	] ++ map Git.fromRef (incompleteExportedTreeish exported)

parseExportLog :: String -> Maybe ExportLog
parseExportLog s = case words s of
	(et:u:it) -> Just $
		ExportLog (Exported (Git.Ref et) (map Git.Ref it)) (toUUID u)
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
