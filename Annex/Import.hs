{- git-annex import from remotes
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Import (
	RemoteTrackingBranch(..),
	mkRemoteTrackingBranch,
	ImportTreeConfig(..),
	ImportCommitConfig(..),
	buildImportCommit,
	buildImportTrees
) where

import Annex.Common
import Types.Import
import qualified Types.Remote as Remote
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

newtype RemoteTrackingBranch = RemoteTrackingBranch
	{ fromRemoteTrackingBranch :: Ref }
	deriving (Show, Eq)

{- Makes a remote tracking branch corresponding to a local branch. 
 - Note that the local branch does not have to exist yet. -}
mkRemoteTrackingBranch :: Remote -> Ref -> RemoteTrackingBranch
mkRemoteTrackingBranch remote ref = RemoteTrackingBranch $
	Git.Ref.underBase ("refs/remotes/" ++ Remote.name remote) ref

{- Configures how to build an import tree. -}
data ImportTreeConfig
	= ImportTree
	-- ^ Import the tree as-is from the remote.
	| ImportSubTree TopFilePath Sha
	-- ^ Import a tree from the remote and graft it into a subdirectory
	-- of the existing tree whose Sha is provided, replacing anything
	-- that was there before.
	deriving (Show)

{- Configures how to build an import commit. -}
data ImportCommitConfig = ImportCommitConfig
	{ importCommitParent :: Maybe Sha
	-- ^ Commit to use as a parent of the import commit.
	, importCommitMode :: Git.Branch.CommitMode
	, importCommitMessage :: String
	}

{- Builds a commit for an import from a special remote. 
 -
 - When a remote provided a history of versions of files,
 - builds a corresponding tree of git commits.
 -
 - When there are no changes to commit (ie, the imported tree is the same
 - as the tree in the importCommitParent), returns Nothing.
 -
 - After importing from a remote, exporting the same thing back to the
 - remote should be a no-op. So, the export log and database are
 - updated to reflect the imported tree.
 -
 - This does not download any content from a remote. But since it needs the
 - Key of imported files to be known, its caller will have to first download
 - new files in order to generate keys for them.
 -}
buildImportCommit
	:: Remote
	-> ImportTreeConfig
	-> ImportCommitConfig
	-> ImportableContents Key
	-> Annex (Maybe Ref)
buildImportCommit remote importtreeconfig importcommitconfig importable =
	case importCommitParent importcommitconfig of
		Nothing -> go emptyTree Nothing
		Just basecommit -> inRepo (Git.Ref.tree basecommit) >>= \case
			Nothing -> go emptyTree Nothing
			Just origtree -> go origtree (Just basecommit)
  where
	basetree = case importtreeconfig of
		ImportTree -> emptyTree
		ImportSubTree _ sha -> sha
	subdir = case importtreeconfig of
		ImportTree -> Nothing
		ImportSubTree dir _ -> Just dir
	
	go origtree basecommit = do
		imported@(History finaltree _) <-
			buildImportTrees basetree subdir importable
		mkcommits origtree basecommit imported >>= \case
			Nothing -> return Nothing
			Just finalcommit -> do
				updateexportdb finaltree
				updateexportlog finaltree
				return (Just finalcommit)

	mkcommits origtree basecommit (History importedtree hs) = do
		parents <- catMaybes <$> mapM (mkcommits origtree basecommit) hs
		if importedtree == origtree && null parents
			then return Nothing -- no changes to commit
			else do
				let commitparents = if null parents
					then catMaybes [basecommit]
					else parents
				commit <- inRepo $ Git.Branch.commitTree
					(importCommitMode importcommitconfig)
					(importCommitMessage importcommitconfig)
					commitparents
					importedtree
				return (Just commit)

	updateexportdb importedtree = 
		withExclusiveLock (gitAnnexExportLock (Remote.uuid remote)) $ do
			db <- openDb (Remote.uuid remote)
			prevtree <- liftIO $ fromMaybe emptyTree
				<$> getExportTreeCurrent db
			when (importedtree /= prevtree) $ do
				updateExportTree db prevtree importedtree
				liftIO $ recordExportTreeCurrent db importedtree
				-- TODO: addExportedLocation etc
				liftIO $ flushDbQueue db
	
	updateexportlog importedtree = do
		old <- getExport (Remote.uuid remote)
		recordExport (Remote.uuid remote) $ ExportChange
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
