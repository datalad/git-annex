{- git-annex export log (also used to log imports)
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Export (
	Exported,
	mkExported,
	ExportParticipants,
	ExportChange(..),
	getExport,
	exportedTreeishes,
	incompleteExportedTreeishes,
	recordExportBeginning,
	recordExportUnderway,
	recordExport,
	logExportExcluded,
	getExportExcluded,
) where

import qualified Data.Map as M

import Annex.Common
import qualified Annex.Branch
import qualified Git
import Git.Sha
import Git.FilePath
import Logs
import Logs.Export.Pure
import Logs.MapLog
import Logs.File
import qualified Git.LsTree
import qualified Git.Tree
import Annex.UUID

import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.Char

-- | Get what's been exported to a special remote.
getExport :: UUID -> Annex [Exported]
getExport remoteuuid = nub . mapMaybe get . M.toList . simpleMap 
	. parseExportLog
	<$> Annex.Branch.get exportLog
  where
	get (ep, exported)
		| exportTo ep == remoteuuid = Just exported
		| otherwise = Nothing

-- | Record the beginning of an export, to allow cleaning up from
-- interrupted exports.
--
-- This is called before any changes are made to the remote.
recordExportBeginning :: UUID -> Git.Ref -> Annex ()
recordExportBeginning remoteuuid newtree = do
	c <- currentVectorClock
	u <- getUUID
	let ep = ExportParticipants { exportFrom = u, exportTo = remoteuuid }
	old <- fromMaybe (mkExported emptyTree [])
		. M.lookup ep . simpleMap 
		. parseExportLog
		<$> Annex.Branch.get exportLog
	let new = updateIncompleteExportedTreeish old (nub (newtree:incompleteExportedTreeishes [old]))
	Annex.Branch.change exportLog $
		buildExportLog 
			. changeMapLog c ep new
			. parseExportLog
	recordExportTreeish newtree

-- Grade a tree ref into the git-annex branch. This is done
-- to ensure that it's available later, when getting exported files
-- from the remote. Since that could happen in another clone of the
-- repository, the tree has to be kept available, even if it
-- doesn't end up being merged into the master branch.
recordExportTreeish :: Git.Ref -> Annex ()
recordExportTreeish t = 
	Annex.Branch.rememberTreeish t (asTopFilePath "export.tree")

-- | Record that an export to a special remote is under way.
--
-- This is called before an export begins uploading new files to the
-- remote, but after it's cleaned up any files that need to be deleted
-- from the old treeish.
--
-- Any entries in the log for the oldTreeish will be updated to the
-- newTreeish. This way, when multiple repositories are exporting to
-- the same special remote, there's no conflict as long as they move
-- forward in lock-step.
recordExportUnderway :: UUID -> ExportChange -> Annex ()
recordExportUnderway remoteuuid ec = do
	c <- currentVectorClock
	hereuuid <- getUUID
	let ep = ExportParticipants { exportFrom = hereuuid, exportTo = remoteuuid }
	let exported = mkExported (newTreeish ec) []
	Annex.Branch.change exportLog $
		buildExportLog
			. changeMapLog c ep exported 
			. M.mapWithKey (updateForExportChange remoteuuid ec c hereuuid)
			. parseExportLog

-- Record information about the export to the git-annex branch.
--
-- This is equivilant to recordExportBeginning followed by
-- recordExportUnderway, but without the ability to clean up from
-- interrupted exports.
recordExport :: UUID -> Git.Ref -> ExportChange -> Annex ()
recordExport remoteuuid tree ec = do
	when (oldTreeish ec /= [tree]) $
		recordExportTreeish tree
	recordExportUnderway remoteuuid ec

logExportExcluded :: UUID -> ((Git.Tree.TreeItem -> IO ()) -> Annex a) -> Annex a
logExportExcluded u a = do
	logf <- fromRepo $ gitAnnexExportExcludeLog u
	withLogHandle logf $ \logh -> do
		liftIO $ hSetNewlineMode logh noNewlineTranslation
		a (writer logh)
  where
	writer logh = hPutStrLn logh
		. Git.LsTree.formatLsTree
		. Git.Tree.treeItemToLsTreeItem

getExportExcluded :: UUID -> Annex [Git.Tree.TreeItem]
getExportExcluded u = do
	logf <- fromRepo $ gitAnnexExportExcludeLog u
	liftIO $ catchDefaultIO [] $ exportExcludedParser
		<$> L.readFile (fromRawFilePath logf)
  where

exportExcludedParser :: L.ByteString -> [Git.Tree.TreeItem]
exportExcludedParser = map Git.Tree.lsTreeItemToTreeItem
	. rights
	. map (Git.LsTree.parseLsTree (Git.LsTree.LsTreeLong False))
	. L.split (fromIntegral $ ord '\n')
