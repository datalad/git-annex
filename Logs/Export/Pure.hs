{- git-annex export log (also used to log imports), pure operations
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Export.Pure (
	Exported,
	mkExported,
	updateExportedTreeish,
	updateIncompleteExportedTreeish,
	ExportParticipants(..),
	ExportChange(..),
	exportedTreeishes,
	incompleteExportedTreeishes,
	parseExportLog,
	buildExportLog,
	updateForExportChange,
) where

import Annex.Common
import qualified Git
import Logs.MapLog

import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

-- This constuctor is not itself exported to other modules, to enforce
-- consistent use of exportedTreeishes.
data Exported = Exported
	{ exportedTreeish :: Git.Ref
	, incompleteExportedTreeish :: [Git.Ref]
	}
	deriving (Eq, Show)

mkExported :: Git.Ref -> [Git.Ref] -> Exported
mkExported = Exported

updateExportedTreeish :: Exported -> Git.Ref -> Exported
updateExportedTreeish ex t = ex { exportedTreeish = t }

updateIncompleteExportedTreeish :: Exported -> [Git.Ref] -> Exported
updateIncompleteExportedTreeish ex t = ex { incompleteExportedTreeish = t }

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

-- Used when recording that an export is under way.
-- Any LogEntry for the oldTreeish will be updated to the newTreeish.
-- This way, when multiple repositories are exporting to
-- the same special remote, there's no conflict as long as they move
-- forward in lock-step.
updateForExportChange :: UUID -> ExportChange -> VectorClock -> UUID -> ExportParticipants -> LogEntry Exported -> LogEntry Exported
updateForExportChange remoteuuid ec c hereuuid ep le@(LogEntry _ exported@(Exported { exportedTreeish = t }))
	| hereuuid == exportFrom ep || remoteuuid /= exportTo ep || t `notElem` oldTreeish ec = le
	| otherwise = LogEntry c (exported { exportedTreeish = newTreeish ec })
