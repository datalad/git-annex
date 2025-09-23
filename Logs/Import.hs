{- git-annex import logs
 -
 - Copyright 2023-2025 Joey Hess <id@joeyh.name>
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
import Logs.Export

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.Set as S

{- Records the sha of a tree that contains hashes of ContentIdentifiers
 - that were imported from a remote.
 -
 - The sha is on the first line of the log file, and following it
 - is a line with the the currently exported treeishs, and then a line with
 - the incomplete exported treeishes.
 -}
recordContentIdentifierTree :: UUID -> Sha -> Annex ()
recordContentIdentifierTree u t = do
	l <- calcRepo' (gitAnnexImportLog u)
	exported <- getExport u
	writeLogFile l $ unlines
		[ fromRef t
		, unwords $ map fromRef $ exportedTreeishes exported
		, unwords $ map fromRef $ incompleteExportedTreeishes exported
		]

{- Gets the ContentIdentifier tree last recorded for a remote.
 -
 - This returns Nothing if no tree was recorded yet. 
 -
 - It also returns Nothing when there have been changes to what is exported
 - to the remote since the tree was recorded. That avoids a problem where
 - diffing from the current Contentidentifier tree to the previous tree
 - would miss changes that were made to a remote by an export, but were
 - later undone manually. For example, if a file was exported to the remote,
 - and then the file was manually removed from the remote, the current tree
 - would not contain the file, and neither would the previous tree.
 - So diffing between the trees would miss that removal. The removed
 - file would then remain in the imported tree.
 -}
getContentIdentifierTree :: UUID -> Annex (Maybe Sha)
getContentIdentifierTree u = do
	l <- calcRepo' (gitAnnexImportLog u)
	-- This is safe because the log file is written atomically.
	ls <- calcLogFileUnsafe l [] (\v ls -> L.toStrict v : ls)
	exported <- getExport u
	return $ case reverse ls of
		-- Subsequent lines are ignored. This leaves room for future
		-- expansion of what is logged.
		(a:b:c:_) -> do
			t <- extractSha a
			exportedtreeishs <- mapM extractSha (S8.words b)
			incompleteexportedtreeishs <- mapM extractSha (S8.words c)
			if same exportedtreeishs (exportedTreeishes exported) && 
			   same incompleteexportedtreeishs (incompleteExportedTreeishes exported)
				then Just t
				else Nothing
		_ -> Nothing
  where
	same l1 l2 = S.fromList l1 == S.fromList l2
