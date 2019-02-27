{- git-annex import from remotes
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Import (
	RemoteTrackingBranch(..),
	mkRemoteTrackingBranch,
	ImportTreeConfig(..),
	ImportCommitConfig(..),
	buildImportCommit,
	buildImportTrees,
	downloadImport
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
import Annex.Content
import Backend
import Types.Key
import Types.KeySource
import Utility.Metered
import Utility.DataUnits
import Logs.Export
import Logs.ContentIdentifier
import Logs.Location
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as CID

import Control.Concurrent.STM
import qualified Data.Map.Strict as M

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
			db <- Export.openDb (Remote.uuid remote)
			prevtree <- liftIO $ fromMaybe emptyTree
				<$> Export.getExportTreeCurrent db
			when (importedtree /= prevtree) $ do
				Export.updateExportTree db prevtree importedtree
				liftIO $ Export.recordExportTreeCurrent db importedtree
				-- TODO: addExportedLocation etc
			Export.closeDb db
	
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

{- Downloads all new ContentIdentifiers. Supports concurrency when enabled.
 -
 - If any download fails, the whole thing fails, but it will resume where
 - it left off.
 -}
downloadImport :: Remote -> ImportTreeConfig -> ImportableContents (ContentIdentifier, ByteSize) -> Annex (Maybe (ImportableContents Key))
downloadImport remote importtreeconfig importablecontents = do
	-- This map is used to remember content identifiers that
	-- were just downloaded, before they have necessarily been
	-- stored in the database. This way, if the same content
	-- identifier appears multiple times in the
	-- importablecontents (eg when it has a history), 
	-- they will only be downloaded once.
	cidmap <- liftIO $ newTVarIO M.empty
	bracket CID.openDb CID.closeDb (go cidmap importablecontents)
	-- TODO really support concurrency; avoid donwloading the same
	-- ContentIdentifier twice.
  where
	go cidmap (ImportableContents l h) db = do
		l' <- mapM (download cidmap db) l
		if any isNothing l'
			then return Nothing
			else do
				h' <- mapM (\ic -> go cidmap ic db) h
				if any isNothing h'
					then return Nothing
					else return $ Just $
						ImportableContents
							(catMaybes l')
							(catMaybes h')
	
	download cidmap db (loc, (cid, sz)) = getcidkey cidmap db cid >>= \case
		(k:_) -> return $ Just (loc, k)
		[] -> checkDiskSpaceToGet tmpkey Nothing $
			withTmp tmpkey $ \tmpfile ->
				Remote.retrieveExportWithContentIdentifier ia loc cid tmpfile (ingestkey loc tmpfile) p >>= \case
					Just k -> do
						recordcidkey cidmap db cid k
						logStatus k InfoPresent
						logChange k (Remote.uuid remote) InfoPresent
						return $ Just (loc, k)
					Nothing -> return Nothing
	  where
		-- TODO progress bar
		p = nullMeterUpdate
		ia = Remote.importActions remote
		tmpkey = importKey cid sz
	
	ingestkey loc tmpfile = do
		f <- fromRepo $ fromTopFilePath $ locworktreefilename loc
		backend <- chooseBackend f
		let ks = KeySource
			{ keyFilename = f
			, contentLocation = tmpfile
			, inodeCache = Nothing
			}
		genKey ks backend >>= \case
			Nothing -> return Nothing
			Just (k, _) ->
				tryNonAsync (moveAnnex k tmpfile) >>= \case
					Right True -> return (Just k)
					_ -> return Nothing

	locworktreefilename loc = asTopFilePath $ case importtreeconfig of
		ImportTree -> fromImportLocation loc
		ImportSubTree subdir _ ->
			getTopFilePath subdir </> fromImportLocation loc

	getcidkey cidmap db cid = liftIO $
		CID.getContentIdentifierKeys db (Remote.uuid remote) cid >>= \case
			[] -> atomically $
				maybeToList . M.lookup cid <$> readTVar cidmap
			l -> return l

	recordcidkey cidmap db cid k = do
		liftIO $ atomically $ modifyTVar' cidmap $
			M.insert cid k
		liftIO $ CID.recordContentIdentifier db (Remote.uuid remote) cid k
		recordContentIdentifier (Remote.uuid remote) cid k

{- Temporary key used for import of a ContentIdentifier while downloading
 - content, before generating its real key. -}
importKey :: ContentIdentifier -> Integer -> Key
importKey (ContentIdentifier cid) size = stubKey
	{ keyName = cid
	, keyVariety = OtherKey "CID"
	, keySize = Just size
	}
