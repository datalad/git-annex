{- git-annex import from remotes
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Import (buildImportCommit) where

import Annex.Common
import Types.Import
import Git.Types
import Git.Tree
import Git.Branch
import Git.FilePath
import Annex.Link

{- Builds a commit on top of a basecommit that reflects changes to the
 - content of a remote. When there are no changes to commit, returns Nothing.
 -
 - When a remote provided a history of versions of files,
 - builds a corresponding tree of git commits.
 -
 - After importing from a remote, exporting the same thing back to the
 - remote should be a no-op. So, the export log is updated to reflect the
 - imported tree.
 -
 - The files are imported to the top of the git repository, unless a
 - subdir is specified, then the import will only affect the contents of
 - the subdir.
 -
 - This does not import any content from a remote. But since it needs the
 - Key of imported files to be known, its caller will have to download
 - new files in order to generate keys for them.
 -}
buildImportCommit
	:: Ref
	-> Maybe FilePath
	-> ImportableContents Key
	-> CommitMode
	-> String
	-> Annex (Maybe Ref)
buildImportCommit basecommit subdir importable commitmode commitmessage = do
	go =<< buildImportTrees basetree importable
  where
	go (History importedtree hs) = do
		parents <- mapM go hs
		
		inRepo $ commitTree commitmode commitmessage parents tree

data History t = History t [History t]

{- Builds a history of git trees reflecting the ImportableContents. -}
buildImportTrees
	:: Maybe FilePath
	-> ImportableContents Key
	-> Annex (History Sha)
buildImportTrees subdir i = History
	<$> go (importableContents i)
	<*> mapM (buildImportTrees subdir basetree) (importableHistory i)
  where
	go ls = do
		is <- mapM mktreeitem ls
		inRepo $ recordTree (treeItemsToTree is)
	mktreeitem (loc, k) = do
		let lf = fromImportLocation loc
		let topf = asTopFilePath $ maybe lf (</> lf) subdir
		relf <- fromRepo $ fromTopFilePath topf
		symlink <- calcRepo $ gitAnnexLink relf k
		linksha <- hashSymlink symlink
		return $ TreeItem topf (fromTreeItemType TreeSymlink) linksha
