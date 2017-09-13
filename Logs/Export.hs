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
import Git.Sha
import Git.FilePath
import Logs
import Logs.MapLog
import Annex.UUID

data Exported = Exported
	{ exportedTreeish :: Git.Ref
	, incompleteExportedTreeish :: [Git.Ref]
	}
	deriving (Eq, Show)

data ExportParticipants = ExportParticipants
	{ exportFrom :: UUID
	, exportTo :: UUID
	}
	deriving (Eq, Ord)

data ExportChange = ExportChange
	{ oldTreeish :: [Git.Ref]
	, newTreeish :: Git.Ref
	}

-- | Get what's been exported to a special remote.
--
-- If the list contains multiple items, there was an export conflict,
-- and different trees were exported to the same special remote.
getExport :: UUID -> Annex [Exported]
getExport remoteuuid = nub . mapMaybe get . M.toList . simpleMap 
	. parseExportLog
	<$> Annex.Branch.get exportLog
  where
	get (ep, exported)
		| exportTo ep == remoteuuid = Just exported
		| otherwise = Nothing

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
	let ep = ExportParticipants { exportFrom = u, exportTo = remoteuuid }
	let exported = Exported (newTreeish ec) []
	Annex.Branch.change exportLog $
		showExportLog
			. changeMapLog c ep exported 
			. M.mapWithKey (updateothers c u)
			. parseExportLog
  where
	updateothers c u ep le@(LogEntry _ exported@(Exported { exportedTreeish = t }))
		| u == exportFrom ep || remoteuuid /= exportTo ep || t `notElem` oldTreeish ec = le
		| otherwise = LogEntry c (exported { exportedTreeish = newTreeish ec })

-- | Record the beginning of an export, to allow cleaning up from
-- interrupted exports.
--
-- This is called before any changes are made to the remote.
recordExportBeginning :: UUID -> Git.Ref -> Annex ()
recordExportBeginning remoteuuid newtree = do
	c <- liftIO currentVectorClock
	u <- getUUID
	let ep = ExportParticipants { exportFrom = u, exportTo = remoteuuid }
	old <- fromMaybe (Exported emptyTree [])
		. M.lookup ep . simpleMap 
		. parseExportLog
		<$> Annex.Branch.get exportLog
	let new = old { incompleteExportedTreeish = nub (newtree:incompleteExportedTreeish old) }
	Annex.Branch.change exportLog $
		showExportLog 
			. changeMapLog c ep new
			. parseExportLog
	Annex.Branch.graftTreeish newtree (asTopFilePath "export.tree")

parseExportLog :: String -> MapLog ExportParticipants Exported
parseExportLog = parseMapLog parseExportParticipants parseExported

showExportLog :: MapLog ExportParticipants Exported -> String
showExportLog = showMapLog formatExportParticipants formatExported

formatExportParticipants :: ExportParticipants -> String
formatExportParticipants ep = 
	fromUUID (exportFrom ep) ++ ':' : fromUUID (exportTo ep)

parseExportParticipants :: String -> Maybe ExportParticipants
parseExportParticipants s = case separate (== ':') s of
	("",_) -> Nothing
	(_,"") -> Nothing
	(f,t) -> Just $ ExportParticipants
		{ exportFrom = toUUID f
		, exportTo = toUUID t
		}
formatExported :: Exported -> String
formatExported exported = unwords $ map Git.fromRef $
	exportedTreeish exported : incompleteExportedTreeish exported

parseExported :: String -> Maybe Exported
parseExported s = case words s of
	(et:it) -> Just $ Exported (Git.Ref et) (map Git.Ref it)
	_ -> Nothing
