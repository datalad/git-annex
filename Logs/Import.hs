{- git-annex import logs
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Import (
	recordContentIdentifierTree,
	getContentIdentifierTree
) where

import Annex.Common
import Git.Types
import Git.Sha
import Logs.File

import qualified Data.ByteString.Lazy as L

{- Records the sha of a tree that contains hashes of ContentIdentifiers
 - that were imported from a remote. -}
recordContentIdentifierTree :: UUID -> Sha -> Annex ()
recordContentIdentifierTree u t = do
	l <- calcRepo' (gitAnnexImportLog u)
	writeLogFile l (fromRef t)

{- Gets the tree last recorded for a remote. -}
getContentIdentifierTree :: UUID -> Annex (Maybe Sha)
getContentIdentifierTree u = do
	l <- calcRepo' (gitAnnexImportLog u)
	-- This is safe because the log file is written atomically.
	calcLogFileUnsafe l Nothing update
  where
	update l Nothing = extractSha (L.toStrict l)
	-- Subsequent lines are ignored. This leaves room for future
	-- expansion of what is logged.
	update _l (Just l) = Just l
