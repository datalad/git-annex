{- git-annex import from remotes
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Import (buildImportCommit, buildImportTrees) where

import Annex.Common
import Types.Import
import Types.Remote (uuid)
import Git.Types
import Git.Tree
import Git.Sha
import Git.FilePath
import qualified Git.Ref
import qualified Git.Branch
import qualified Annex
import Annex.Link
import Annex.LockFile
import Logs.Export
import Database.Export

{- Builds a commit on top of a basecommit that reflects changes to the
 - content of a remote. When there are no changes to commit, returns Nothing.
 -
 - When a remote provided a history of versions of files,
 - builds a corresponding tree of git commits.
 -
 - After importing from a remote, exporting the same thing back to the
 - remote should be a no-op. So, the export log and database are
 - updated to reflect the imported tree.
 -
 - The files are imported to the top of the git repository, unless a
 - subdir is specified, then the import will only affect the contents of
 - the subdir.
 -
 - This does not import any content from a remote. But since it needs the
 - Key of imported files to be known, its caller will have to first download
 - new files in order to generate keys for them.
 -}
buildImportCommit
	:: Remote
	-> Ref
	-> Maybe TopFilePath
	-> ImportableContents Key
	-> Git.Branch.CommitMode
	-> String
	-> Annex (Either String (Maybe Ref))
buildImportCommit remote basecommit subdir importable commitmode commitmessage =
	inRepo (Git.Ref.tree basecommit) >>= \case
		Nothing -> return $
			Left $ "Cannot find tree for " ++ fromRef basecommit
		Just basetree -> do
			imported@(History finaltree _) <-
				buildImportTrees basetree subdir importable
			mkcommits basetree imported >>= \case
				Nothing -> return (Right Nothing)
				Just finalcommit -> do
					updateexportdb finaltree
					updateexportlog finaltree
					return (Right (Just finalcommit))
  where
	mkcommits basetree (History importedtree hs) = do
		parents <- catMaybes <$> mapM (mkcommits basetree) hs
		if basetree == importedtree && null parents
			then return Nothing
			else do
				commit <- inRepo $ Git.Branch.commitTree commitmode commitmessage parents importedtree
				return (Just commit)
	updateexportdb importedtree = 
		withExclusiveLock (gitAnnexExportLock (uuid remote)) $ do
			db <- openDb (uuid remote)
			prevtree <- liftIO $ fromMaybe emptyTree
				<$> getExportTreeCurrent db
			when (importedtree /= prevtree) $ do
				updateExportTree db prevtree importedtree
				liftIO $ recordExportTreeCurrent db importedtree
				-- TODO: addExportedLocation etc
				liftIO $ flushDbQueue db
	updateexportlog importedtree = do
		old <- getExport (uuid remote)
		recordExport (uuid remote) $ ExportChange
			{ oldTreeish = exportedTreeishes old
			, newTreeish = importedtree
			}

data History t = History t [History t]
	deriving (Show)

{- Builds a history of git trees reflecting the ImportableContents.
 -
 - When a subdir is provided, imported tree is grafted into the basetree at
 - that location, replacing any object that was there.
 -}
buildImportTrees
	:: Ref
	-> Maybe TopFilePath
	-> ImportableContents Key
	-> Annex (History Sha)
buildImportTrees basetree msubdir importable = History
	<$> (go (importableContents importable) =<< Annex.gitRepo)
	<*> mapM (buildImportTrees basetree msubdir) (importableHistory importable)
  where
	go ls repo = withMkTreeHandle repo $ \hdl -> do
		importtree <- liftIO . recordTree' hdl 
			. treeItemsToTree
			=<< mapM mktreeitem ls
		case msubdir of
			Nothing -> return importtree
			Just subdir -> liftIO $ 
				graftTree' importtree subdir basetree repo hdl
	mktreeitem (loc, k) = do
		let lf = fromImportLocation loc
		let treepath = asTopFilePath lf
		let topf = asTopFilePath $
			maybe lf (\sd -> getTopFilePath sd </> lf) msubdir
		relf <- fromRepo $ fromTopFilePath topf
		symlink <- calcRepo $ gitAnnexLink relf k
		linksha <- hashSymlink symlink
		return $ TreeItem treepath (fromTreeItemType TreeSymlink) linksha
