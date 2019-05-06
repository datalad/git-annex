{- git-annex import from remotes
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Import (
	importTree,
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
import Git.History
import qualified Git.Ref
import qualified Git.Branch
import qualified Annex
import Annex.Link
import Annex.LockFile
import Annex.Content
import Annex.Export
import Annex.RemoteTrackingBranch
import Command
import Backend
import Config
import Types.Key
import Types.KeySource
import Messages.Progress
import Utility.DataUnits
import Logs.Export
import Logs.Location
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as CIDDb
import qualified Logs.ContentIdentifier as CIDLog

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
	{ importCommitTracking :: Maybe Sha
	-- ^ Current commit on the remote tracking branch.
	, importCommitMode :: Git.Branch.CommitMode
	, importCommitMessage :: String
	}

{- Buils a commit for an import from a special remote.
 -
 - When there are no changes to make (importCommitTracking
 - already matches what was imported), returns Nothing.
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
	case importCommitTracking importcommitconfig of
		Nothing -> go Nothing
		Just trackingcommit -> inRepo (Git.Ref.tree trackingcommit) >>= \case
			Nothing -> go Nothing
			Just _ -> go (Just trackingcommit)
  where
	basetree = case importtreeconfig of
		ImportTree -> emptyTree
		ImportSubTree _ sha -> sha
	subdir = case importtreeconfig of
		ImportTree -> Nothing
		ImportSubTree dir _ -> Just dir
	
	go trackingcommit = do
		imported@(History finaltree _) <-
			buildImportTrees basetree subdir importable
		buildImportCommit' importcommitconfig trackingcommit imported >>= \case
			Just finalcommit -> do
				updatestate finaltree
				return (Just finalcommit)
			Nothing -> return Nothing
	
	updatestate committedtree = do
		importedtree <- case subdir of
			Nothing -> pure committedtree
			Just dir -> 
				let subtreeref = Ref $
					fromRef committedtree ++ ":" ++ getTopFilePath dir
				in fromMaybe emptyTree
					<$> inRepo (Git.Ref.tree subtreeref)
		updateexportdb importedtree
		oldexport <- updateexportlog importedtree
		updatelocationlog oldexport importedtree
	
	updateexportdb importedtree = do
		db <- Export.openDb (Remote.uuid remote)
		Export.writeLockDbWhile db $ do
			prevtree <- liftIO $ fromMaybe emptyTree
				<$> Export.getExportTreeCurrent db
			when (importedtree /= prevtree) $ do
				Export.updateExportDb db prevtree importedtree
				liftIO $ Export.recordExportTreeCurrent db importedtree
		Export.closeDb db
	
	updateexportlog importedtree = do
		oldexport <- getExport (Remote.uuid remote)
		recordExport (Remote.uuid remote) $ ExportChange
			{ oldTreeish = exportedTreeishes oldexport
			, newTreeish = importedtree
			}
		return oldexport

	-- downloadImport takes care of updating the location log
	-- for the local repo when keys are downloaded, and also updates
	-- the location log for the remote for keys that are present in it.
	-- That leaves updating the location log for the remote for keys
	-- that have had the last copy of their content removed from it.
	--
	-- This must run after the export database has been updated
	-- and flushed to disk, so it can query it.
	updatelocationlog oldexport finaltree = do
		let stillpresent db k = liftIO $ not . null
			<$> Export.getExportedLocation db k
		let updater db oldkey _newkey _ = case oldkey of
			Just (AnnexKey k) -> unlessM (stillpresent db k) $
				logChange k (Remote.uuid remote) InfoMissing
			Just (GitKey _) -> noop
			Nothing -> noop
		db <- Export.openDb (Remote.uuid remote)
		forM_ (exportedTreeishes oldexport) $ \oldtree ->
			Export.runExportDiffUpdater updater db oldtree finaltree
		Export.closeDb db

buildImportCommit' :: ImportCommitConfig -> Maybe Sha -> History Sha -> Annex (Maybe Sha)
buildImportCommit' importcommitconfig mtrackingcommit imported@(History ti _) =
	case mtrackingcommit of
		Nothing -> Just <$> mkcommits imported
		Just trackingcommit -> do
			-- Get history of tracking branch to at most
			-- one more level deep than what was imported,
			-- so we'll have enough history to compare,
			-- but not spend too much time getting it.
			let maxdepth = succ (historyDepth imported)
			inRepo (getHistoryToDepth maxdepth trackingcommit)
				>>= go trackingcommit
  where
	go _ Nothing = Just <$> mkcommits imported
	go trackingcommit (Just h)
		-- If the tracking branch head is a merge commit
		-- with a tree that matches the head of the history,
		-- and one side of the merge matches the history,
		-- nothing new needs to be committed.
		| t == ti && any (sametodepth imported) (S.toList s) = return Nothing
		-- If the tracking branch matches the history,
		-- nothing new needs to be committed.
		-- (This is unlikely to happen.)
		| sametodepth imported h' = return Nothing
		| otherwise = do
			importedcommit <- case getRemoteTrackingBranchImportHistory h of
				Nothing ->
					mkcommits imported
				Just oldimported@(History oldhc _) -> do
					let oldimportedtrees = mapHistory historyCommitTree oldimported
					mknewcommits oldhc oldimportedtrees imported
			Just <$> makeRemoteTrackingBranchMergeCommit'
				trackingcommit importedcommit
	  where
		h'@(History t s) = mapHistory historyCommitTree h

	sametodepth a b = a == truncateHistoryToDepth (historyDepth a) b

	mkcommit parents tree = inRepo $ Git.Branch.commitTree
		(importCommitMode importcommitconfig)
		(importCommitMessage importcommitconfig)
		parents
		tree

	mkcommits (History importedtree hs) = do
		parents <- mapM mkcommits (S.toList hs)
		mkcommit parents importedtree

	-- Reuse the commits from the old imported History when possible.
	mknewcommits oldhc old new@(History importedtree hs)
		| old == new = return $ historyCommit oldhc
		| otherwise = do
			parents <- mapM (mknewcommits oldhc old) (S.toList hs)
			mkcommit parents importedtree

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
	<$> (buildtree (importableContents importable) =<< Annex.gitRepo)
	<*> buildhistory
  where
	buildhistory = S.fromList
		<$> mapM (buildImportTrees basetree msubdir)
			(importableHistory importable)
	
	buildtree ls repo = withMkTreeHandle repo $ \hdl -> do
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

{- Downloads all new ContentIdentifiers as needed to generate Keys. 
 - Supports concurrency when enabled.
 -
 - If any download fails, the whole thing fails with Nothing, 
 - but it will resume where it left off.
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
	-- When concurrency is enabled, this set is needed to
	-- avoid two threads both downloading the same content identifier.
	downloading <- liftIO $ newTVarIO S.empty
	withExclusiveLock gitAnnexContentIdentifierLock $
		bracket CIDDb.openDb CIDDb.closeDb $ \db -> do
			CIDDb.needsUpdateFromLog db
				>>= maybe noop (CIDDb.updateFromLog db)
			go False cidmap downloading importablecontents db
  where
	go oldversion cidmap downloading (ImportableContents l h) db = do
		jobs <- forM l $ \i ->
			startdownload cidmap downloading db i oldversion
		l' <- liftIO $ forM jobs $
			either pure (atomically . takeTMVar)
		if any isNothing l'
			then return Nothing
			else do
				h' <- mapM (\ic -> go True cidmap downloading ic db) h
				if any isNothing h'
					then return Nothing
					else return $ Just $
						ImportableContents
							(catMaybes l')
							(catMaybes h')
	
	waitstart downloading cid = liftIO $ atomically $ do
		s <- readTVar downloading
		if S.member cid s
			then retry
			else writeTVar downloading $ S.insert cid s
	
	signaldone downloading cid = liftIO $ atomically $ do
		s <- readTVar downloading
		writeTVar downloading $ S.delete cid s
	
	startdownload cidmap downloading db i@(loc, (cid, _sz)) oldversion = getcidkey cidmap db cid >>= \case
		(k:_) -> return $ Left $ Just (loc, k)
		[] -> do
			job <- liftIO $ newEmptyTMVarIO
			let downloadaction = do
				showStart ("import " ++ Remote.name remote) (fromImportLocation loc)
				when oldversion $
					showNote "old version"
				next $ tryNonAsync (download cidmap db i) >>= \case
					Left e -> next $ do
						warning (show e)
						liftIO $ atomically $
							putTMVar job Nothing
						return False
					Right r -> next $ do
						liftIO $ atomically $
							putTMVar job r
						return True
			commandAction $ bracket_
				(waitstart downloading cid)
				(signaldone downloading cid)
				downloadaction
			return (Right job)
	
	download cidmap db (loc, (cid, sz)) = do
		let rundownload tmpfile p = 
			Remote.retrieveExportWithContentIdentifier ia loc cid tmpfile (mkkey loc tmpfile) p >>= \case
				Just k -> tryNonAsync (moveAnnex k tmpfile) >>= \case
					Right True -> do
						recordcidkey cidmap db cid k
						logStatus k InfoPresent
						logChange k (Remote.uuid remote) InfoPresent
						return $ Just (loc, k)
					_ -> return Nothing
				Nothing -> return Nothing
		checkDiskSpaceToGet tmpkey Nothing $
			withTmp tmpkey $ \tmpfile ->
				metered Nothing tmpkey (return Nothing) $
					const (rundownload tmpfile)
	  where
		ia = Remote.importActions remote
		tmpkey = importKey cid sz
	
	mkkey loc tmpfile = do
		f <- fromRepo $ fromTopFilePath $ locworktreefilename loc
		backend <- chooseBackend f
		let ks = KeySource
			{ keyFilename = f
			, contentLocation = tmpfile
			, inodeCache = Nothing
			}
		fmap fst <$> genKey ks backend

	locworktreefilename loc = asTopFilePath $ case importtreeconfig of
		ImportTree -> fromImportLocation loc
		ImportSubTree subdir _ ->
			getTopFilePath subdir </> fromImportLocation loc

	getcidkey cidmap db cid = liftIO $
		CIDDb.getContentIdentifierKeys db (Remote.uuid remote) cid >>= \case
			[] -> atomically $
				maybeToList . M.lookup cid <$> readTVar cidmap
			l -> return l

	recordcidkey cidmap db cid k = do
		liftIO $ atomically $ modifyTVar' cidmap $
			M.insert cid k
		liftIO $ CIDDb.recordContentIdentifier db (Remote.uuid remote) cid k
		CIDLog.recordContentIdentifier (Remote.uuid remote) cid k

{- Temporary key used for import of a ContentIdentifier while downloading
 - content, before generating its real key. -}
importKey :: ContentIdentifier -> Integer -> Key
importKey (ContentIdentifier cid) size = stubKey
	{ keyName = cid
	, keyVariety = OtherKey "CID"
	, keySize = Just size
	}
