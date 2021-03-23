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
import Logs.MapLog
import Logs.File
import qualified Git.LsTree
import qualified Git.Tree
import Annex.UUID

import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder
import Data.Either
import Data.Char

-- This constuctor is not itself exported to other modules, to enforce
-- consistent use of exportedTreeishes.
data Exported = Exported
	{ exportedTreeish :: Git.Ref
	, incompleteExportedTreeish :: [Git.Ref]
	}
	deriving (Eq, Show)

mkExported :: Git.Ref -> [Git.Ref] -> Exported
mkExported = Exported

-- | Get the list of exported treeishes.
--
-- If the list contains multiple items, there was an export conflict,
-- and different trees were exported to the same special remote.
exportedTreeishes :: [Exported] -> [Git.Ref]
exportedTreeishes = nub . map exportedTreeish

-- | Treeishes that started to be exported, but were not finished.
incompleteExportedTreeishes :: [Exported] -> [Git.Ref]
incompleteExportedTreeishes = concatMap incompleteExportedTreeish

data ExportParticipants = ExportParticipants
	{ exportFrom :: UUID
	, exportTo :: UUID
	}
	deriving (Eq, Ord, Show)

data ExportChange = ExportChange
	{ oldTreeish :: [Git.Ref]
	, newTreeish :: Git.Ref
	}

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
	old <- fromMaybe (Exported emptyTree [])
		. M.lookup ep . simpleMap 
		. parseExportLog
		<$> Annex.Branch.get exportLog
	let new = old { incompleteExportedTreeish = nub (newtree:incompleteExportedTreeish old) }
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
	u <- getUUID
	let ep = ExportParticipants { exportFrom = u, exportTo = remoteuuid }
	let exported = Exported (newTreeish ec) []
	Annex.Branch.change exportLog $
		buildExportLog
			. changeMapLog c ep exported 
			. M.mapWithKey (updateothers c u)
			. parseExportLog
  where
	updateothers c u ep le@(LogEntry _ exported@(Exported { exportedTreeish = t }))
		| u == exportFrom ep || remoteuuid /= exportTo ep || t `notElem` oldTreeish ec = le
		| otherwise = LogEntry c (exported { exportedTreeish = newTreeish ec })

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

parseExportLog :: L.ByteString -> MapLog ExportParticipants Exported
parseExportLog = parseMapLog exportParticipantsParser exportedParser

buildExportLog :: MapLog ExportParticipants Exported -> Builder
buildExportLog = buildMapLog buildExportParticipants buildExported

buildExportParticipants :: ExportParticipants -> Builder
buildExportParticipants ep = 
	buildUUID (exportFrom ep) <> sep <> buildUUID (exportTo ep)
  where
	sep = charUtf8 ':'

exportParticipantsParser :: A.Parser ExportParticipants
exportParticipantsParser = ExportParticipants
	<$> (toUUID <$> A8.takeWhile1 (/= ':'))
	<* A8.char ':'
	<*> (toUUID <$> A8.takeWhile1 (const True))

buildExported :: Exported -> Builder
buildExported exported = go (exportedTreeish exported : incompleteExportedTreeish exported)
  where
	go [] = mempty
	go (r:rs) = rref r <> mconcat [ charUtf8 ' ' <> rref r' | r' <- rs ]
	rref = byteString . Git.fromRef'

exportedParser :: A.Parser Exported
exportedParser = Exported <$> refparser <*> many refparser
  where
	refparser = (Git.Ref <$> A8.takeWhile1 (/= ' ') )
		<* ((const () <$> A8.char ' ') <|> A.endOfInput)

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
