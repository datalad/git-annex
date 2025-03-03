{- git-annex import from remotes
 -
 - Copyright 2019-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Annex.Import (
	ImportTreeConfig(..),
	ImportCommitConfig(..),
	buildImportCommit,
	buildImportTrees,
	recordImportTree,
	canImportKeys,
	ImportResult(..),
	Imported,
	importChanges,
	importKeys,
	makeImportMatcher,
	getImportableContents,
) where

import Annex.Common
import Types.Import
import qualified Types.Remote as Remote
import Git.Types
import Git.Tree
import Git.Sha
import Git.FilePath
import Git.History
import qualified Git.DiffTree
import qualified Git.Ref
import qualified Git.Branch
import qualified Annex
import Annex.Link
import Annex.LockFile
import Annex.Content
import Annex.RemoteTrackingBranch
import Annex.HashObject
import Annex.Transfer
import Annex.CheckIgnore
import Annex.CatFile
import Annex.GitShaKey
import Annex.VectorClock
import Annex.SpecialRemote.Config
import Command
import Backend
import Types.Key
import Types.KeySource
import Messages.Progress
import Utility.DataUnits
import Utility.Metered
import Utility.Hash (sha1s)
import Logs.Import
import Logs.Export
import Logs.Location
import Logs.PreferredContent
import Types.FileMatcher
import Annex.FileMatcher
import qualified Utility.Matcher
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as CIDDb
import qualified Logs.ContentIdentifier as CIDLog
import Backend.Utilities

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteArray.Encoding as BA
#ifdef mingw32_HOST_OS
import qualified System.FilePath.Posix as Posix
#endif

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
	, importCommitMessages :: [String]
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
	-> AddUnlockedMatcher
	-> Imported
	-> Annex (Maybe Ref)
buildImportCommit remote importtreeconfig importcommitconfig addunlockedmatcher imported =
	case importCommitTracking importcommitconfig of
		Nothing -> go Nothing
		Just trackingcommit -> inRepo (Git.Ref.tree trackingcommit) >>= \case
			Nothing -> go Nothing
			Just _ -> go (Just trackingcommit)
  where
	go trackingcommit = do
		(importedtree, updatestate) <- recordImportTree remote importtreeconfig (Just addunlockedmatcher) imported
		buildImportCommit' remote importcommitconfig trackingcommit importedtree >>= \case
			Just finalcommit -> do
				updatestate
				return (Just finalcommit)
			Nothing -> return Nothing

{- Builds a tree for an import from a special remote.
 -
 - Also returns an action that can be used to update 
 - all the other state to record the import.
 -}
recordImportTree
	:: Remote
	-> ImportTreeConfig
	-> Maybe AddUnlockedMatcher
	-> Imported
	-> Annex (History Sha, Annex ())
recordImportTree remote importtreeconfig addunlockedmatcher imported = do
	importedtree@(History finaltree _) <- buildImportTrees basetree subdir addunlockedmatcher imported
	return (importedtree, updatestate finaltree)
  where
	basetree = case importtreeconfig of
		ImportTree -> emptyTree
		ImportSubTree _ sha -> sha
	subdir = case importtreeconfig of
		ImportTree -> Nothing
		ImportSubTree dir _ -> Just dir
	
	updatestate finaltree = do
		importedtree <- case subdir of
			Nothing -> pure finaltree
			Just dir -> 
				let subtreeref = Ref $
					fromRef' finaltree
						<> ":"
						<> fromOsPath (getTopFilePath dir)
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
		recordExport (Remote.uuid remote) importedtree $ ExportChange
			{ oldTreeish = exportedTreeishes oldexport
			, newTreeish = importedtree
			}
		return oldexport

	-- importKeys takes care of updating the location log
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
		let updater db moldkey _newkey _ = case moldkey of
			Just oldkey | not (isGitShaKey oldkey) ->
				unlessM (stillpresent db oldkey) $
					logChange NoLiveUpdate oldkey (Remote.uuid remote) InfoMissing
			_ -> noop
		-- When the remote is versioned, it still contains keys
		-- that are not present in the new tree.
		unless (isVersioning (Remote.config remote)) $ do
			db <- Export.openDb (Remote.uuid remote)
			forM_ (exportedTreeishes oldexport) $ \oldtree ->
				Export.runExportDiffUpdater updater db oldtree finaltree
			Export.closeDb db

buildImportCommit' :: Remote -> ImportCommitConfig -> Maybe Sha -> History Sha -> Annex (Maybe Sha)
buildImportCommit' remote importcommitconfig mtrackingcommit imported@(History ti _) =
	case mtrackingcommit of
		Nothing -> Just <$> mkcommitsunconnected imported
		Just trackingcommit -> do
			-- Get history of tracking branch to at most
			-- one more level deep than what was imported,
			-- so we'll have enough history to compare,
			-- but not spend too much time getting it.
			let maxdepth = succ importeddepth
			inRepo (getHistoryToDepth maxdepth trackingcommit)
				>>= go trackingcommit
  where
	go _ Nothing = Just <$> mkcommitsunconnected imported
	go trackingcommit (Just h)
		-- If the tracking branch head is a merge commit
		-- and one side of the merge matches the history,
		-- nothing new needs to be committed.
		| t == ti && any sametodepth (S.toList s) = return Nothing
		-- If the tracking branch matches the history,
		-- nothing new needs to be committed.
		-- (This is unlikely to happen.)
		| sametodepth h' = return Nothing
		-- If the imported tree is unchanged,
		-- nothing new needs to be committed.
		| otherwise = getLastImportedTree remote >>= \case
			Just (LastImportedTree lasttree)
				| lasttree == ti -> return Nothing
			_ -> gencommit trackingcommit h
	  where
		h'@(History t s) = mapHistory historyCommitTree h
	
	gencommit trackingcommit h = do
		importedcommit <- case getRemoteTrackingBranchImportHistory h of
			Nothing -> mkcommitsunconnected imported
			Just oldimported@(History oldhc _)
				| importeddepth == 1 ->
					mkcommitconnected imported oldimported
				| otherwise -> do
					let oldimportedtrees = mapHistory historyCommitTree oldimported
					mknewcommits oldhc oldimportedtrees imported
		ti' <- addBackExportExcluded remote ti
		Just <$> makeRemoteTrackingBranchMergeCommit'
			trackingcommit importedcommit ti'

	importeddepth = historyDepth imported

	sametodepth b = imported == truncateHistoryToDepth importeddepth b

	mkcommit parents tree = inRepo $ Git.Branch.commitTree
		(importCommitMode importcommitconfig)
		(importCommitMessages importcommitconfig)
		parents
		tree

	-- Start a new history of import commits, not connected to any
	-- prior import commits.
	mkcommitsunconnected (History importedtree hs) = do
		parents <- mapM mkcommitsunconnected (S.toList hs)
		mkcommit parents importedtree

	-- Commit the new history connected with the old history.
	-- Used when the import is not versioned, so the history depth is 1.
	mkcommitconnected (History importedtree _) (History oldhc _) = do
		let parents = [historyCommit oldhc]
		mkcommit parents importedtree

	-- Reuse the commits from the old imported History when possible.
	mknewcommits oldhc old new@(History importedtree hs)
		| old == new = return $ historyCommit oldhc
		| otherwise = do
			parents <- mapM (mknewcommits oldhc old) (S.toList hs)
			mkcommit parents importedtree

{- Builds a history of git trees for an import.
 -
 - When a subdir is provided, the imported tree is grafted into 
 - the basetree at that location, replacing any object that was there.
 -}
buildImportTrees
	:: Ref
	-> Maybe TopFilePath
	-> Maybe AddUnlockedMatcher
	-> Imported
	-> Annex (History Sha)
buildImportTrees basetree msubdir addunlockedmatcher (ImportedFull imported) = 
	buildImportTreesGeneric (convertImportTree addunlockedmatcher) basetree msubdir imported
buildImportTrees basetree msubdir addunlockedmatcher (ImportedDiff (LastImportedTree oldtree) imported) = do
	importtree <- if null (importableContents imported)
		then pure oldtree
		else applydiff
	repo <- Annex.gitRepo
	t <- withMkTreeHandle repo $
		graftImportTree basetree msubdir importtree
	-- Diffing is not currently implemented when the history is not empty.
	return (History t mempty)
  where
	applydiff = do
		let (removed, new) = partition isremoved
			(importableContents imported)
		newtreeitems <- catMaybes <$> mapM mktreeitem new
		let removedfiles = map (mkloc . fst) removed
		inRepo $ adjustTree
			(pure . Just) 
			-- ^ keep files that are not added/removed the same
			newtreeitems
			(\_oldti newti -> newti)
			-- ^ prefer newly added version of file
			removedfiles
			oldtree
	
	mktreeitem (loc, DiffChanged v) = 
		Just <$> mkImportTreeItem addunlockedmatcher msubdir loc v
	mktreeitem (_, DiffRemoved) = 
		pure Nothing

	mkloc = asTopFilePath . fromImportLocation
		
	isremoved (_, v) = v == DiffRemoved

convertImportTree :: Maybe AddUnlockedMatcher -> Maybe TopFilePath -> [(ImportLocation, Either Sha Key)] -> Annex Tree
convertImportTree maddunlockedmatcher msubdir ls = 
	treeItemsToTree <$> mapM (uncurry $ mkImportTreeItem maddunlockedmatcher msubdir) ls

mkImportTreeItem :: Maybe AddUnlockedMatcher -> Maybe TopFilePath -> ImportLocation -> Either Sha Key -> Annex TreeItem
mkImportTreeItem maddunlockedmatcher msubdir loc v = case v of
	Right k -> case maddunlockedmatcher of
		Nothing -> mklink k
		Just addunlockedmatcher -> do
			objfile <- calcRepo (gitAnnexLocation k)
			let mi = MatchingFile FileInfo
				{ contentFile = objfile
				, matchFile = getTopFilePath topf
				, matchKey = Just k
				}
			ifM (checkAddUnlockedMatcher NoLiveUpdate addunlockedmatcher mi)
				( mkpointer k
				, mklink k
				)
				
	Left sha -> 
		return $ TreeItem treepath (fromTreeItemType TreeFile) sha
  where
	lf = fromImportLocation loc
	treepath = asTopFilePath lf
	topf = asTopFilePath $
		maybe lf (\sd -> getTopFilePath sd </> lf) msubdir
	mklink k = do
		relf <- fromRepo $ fromTopFilePath topf
		symlink <- calcRepo $ gitAnnexLink relf k
		linksha <- hashSymlink (fromOsPath symlink)
		return $ TreeItem treepath (fromTreeItemType TreeSymlink) linksha
	mkpointer k = TreeItem treepath (fromTreeItemType TreeFile)
		<$> hashPointerFile k

{- Builds a history of git trees using ContentIdentifiers.
 -
 - These are not the final trees that are generated by the import, which
 - use Keys. The purpose of these trees is to allow quickly determining
 - which files in the import have changed, and which are unchanged, to
 - avoid needing to look up the Keys for unchanged ContentIdentifiers.
 - When the import has a large number of files, that can be slow.
 -}
buildContentIdentifierTree
	:: ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)
	-> Annex (History Sha, M.Map Sha (ContentIdentifier, ByteSize))
buildContentIdentifierTree importable = do
	mv <- liftIO $ newTVarIO M.empty
	r <- buildImportTreesGeneric (convertContentIdentifierTree mv) emptyTree Nothing importable
	m <- liftIO $ atomically $ readTVar mv
	return (r, m)

{- For speed, and to avoid bloating the repository, the ContentIdentifiers
 - are not actually checked into git, instead a sha1 hash is calculated
 - internally.
 -}
convertContentIdentifierTree
	:: TVar (M.Map Sha (ContentIdentifier, ByteSize))
	-> Maybe TopFilePath
	-> [(ImportLocation, (ContentIdentifier, ByteSize))]
	-> Annex Tree
convertContentIdentifierTree mv _ ls = do
	let (tis, ml) = unzip (map mktreeitem ls)
	liftIO $ atomically $ modifyTVar' mv $
		M.union (M.fromList ml)
	return (treeItemsToTree tis)
  where
	mktreeitem (loc, v@((ContentIdentifier cid), _sz)) =
		(TreeItem p mode sha1, (sha1, v))
	  where
		p = asTopFilePath (fromImportLocation loc)
		mode = fromTreeItemType TreeFile
		-- Note that this hardcodes sha1, even if git has started
		-- defaulting to some other checksum method. That should be
		-- ok, hopefully. This checksum never needs to be verified
		-- by git, which is why this does not bother to prefix the
		-- cid with its length, like git would.
		sha1 = Ref $ BA.convertToBase BA.Base16 $ sha1s cid

buildImportTreesGeneric
	:: (Maybe TopFilePath -> [(ImportLocation, v)] -> Annex Tree)
	-> Ref
	-> Maybe TopFilePath
	-> ImportableContentsChunkable Annex v
	-> Annex (History Sha)
buildImportTreesGeneric converttree basetree msubdir (ImportableContentsComplete importable) = do
	repo <- Annex.gitRepo
	withMkTreeHandle repo $ buildImportTreesGeneric' converttree basetree msubdir importable
buildImportTreesGeneric converttree basetree msubdir importable@(ImportableContentsChunked {}) = do
	repo <- Annex.gitRepo
	withMkTreeHandle repo $ \hdl ->
		History
			<$> go hdl
			<*> buildImportTreesHistory converttree basetree msubdir
				(importableHistoryComplete importable) hdl
  where
	go hdl = do
		tree <- gochunks [] (importableContentsChunk importable) hdl
		importtree <- liftIO $ recordTree' hdl tree
		graftImportTree basetree msubdir importtree hdl

	gochunks l c hdl = do
		let subdir = importChunkSubDir $ importableContentsSubDir c
		-- Full directory prefix where the sub tree is located.
		let fullprefix = asTopFilePath $ case msubdir of
			Nothing -> subdir
			Just d ->
#ifdef mingw32_HOST_OS
				toOsPath $ fromOsPath (getTopFilePath d) Posix.</> fromOsPath subdir
#else
				getTopFilePath d </> subdir
#endif
		Tree ts <- converttree (Just fullprefix) $
			map (\(p, i) -> (mkImportLocation p, i))
				(importableContentsSubTree c)
		-- Record this subtree before getting next chunk, this
		-- avoids buffering all the chunks into memory.
		tc <- liftIO $ recordSubTree hdl $
			NewSubTree (asTopFilePath subdir) ts
		importableContentsNextChunk c >>= \case
			Nothing -> return (Tree (tc:l))
			Just c' -> gochunks (tc:l) c' hdl

buildImportTreesGeneric'
	:: (Maybe TopFilePath -> [(ImportLocation, v)] -> Annex Tree)
	-> Ref
	-> Maybe TopFilePath
	-> ImportableContents v
	-> MkTreeHandle
	-> Annex (History Sha)
buildImportTreesGeneric' converttree basetree msubdir importable hdl = History
	<$> buildImportTree converttree basetree msubdir (importableContents importable) hdl
	<*> buildImportTreesHistory converttree basetree msubdir (importableHistory importable) hdl

buildImportTree
	:: (Maybe TopFilePath -> [(ImportLocation, v)] -> Annex Tree)
	-> Ref
	-> Maybe TopFilePath
	-> [(ImportLocation, v)]
	-> MkTreeHandle
	-> Annex Sha
buildImportTree converttree basetree msubdir ls hdl = do
	importtree <- liftIO . recordTree' hdl =<< converttree msubdir ls
	graftImportTree basetree msubdir importtree hdl

graftImportTree
	:: Ref
	-> Maybe TopFilePath
	-> Sha
	-> MkTreeHandle
	-> Annex Sha
graftImportTree basetree msubdir tree hdl = case msubdir of
	Nothing -> return tree
	Just subdir -> inRepo $ \repo ->
		graftTree' tree subdir basetree repo hdl

buildImportTreesHistory
	:: (Maybe TopFilePath -> [(ImportLocation, v)] -> Annex Tree)
	-> Ref
	-> Maybe TopFilePath
	-> [ImportableContents v]
	-> MkTreeHandle
	-> Annex (S.Set (History Sha))
buildImportTreesHistory converttree basetree msubdir history hdl = S.fromList
	<$> mapM (\ic -> buildImportTreesGeneric' converttree basetree msubdir ic hdl) history

canImportKeys :: Remote -> Bool -> Bool
canImportKeys remote importcontent =
	importcontent || isJust (Remote.importKey ia)
  where
	ia = Remote.importActions remote

-- Result of an import. ImportUnfinished indicates that some file failed to
-- be imported. Running again should resume where it left off.
data ImportResult t
	= ImportFinished t
	| ImportUnfinished

data Diffed t
	= DiffChanged t
	| DiffRemoved
	deriving (Eq)

data Imported
	= ImportedFull (ImportableContentsChunkable Annex (Either Sha Key))
	| ImportedDiff LastImportedTree (ImportableContents (Diffed (Either Sha Key)))

newtype LastImportedTree = LastImportedTree Sha

{- Diffs between the previous and current ContentIdentifier trees, and 
 - runs importKeys on only the changed files.
 -
 - This will download the same content as if importKeys were run on all
 - files, but this speeds it up significantly when there are a lot of files
 - and only a few have changed. importKeys has to look up each
 - ContentIdentifier to see if a Key is known for it. This avoids doing
 - that lookup on files that have not changed.
 -
 - Diffing is not currently implemented when there is a History.
 -}
importChanges
	:: Remote
	-> ImportTreeConfig
	-> Bool
	-> Bool
	-> ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)
	-> Annex (ImportResult Imported)
importChanges remote importtreeconfig importcontent thirdpartypopulated importablecontents = do
	((History currcidtree currhistory), cidtreemap) <- buildContentIdentifierTree importablecontents
	-- diffimport below does not handle history, so when there is
	-- history, do a full import.
	if not (S.null currhistory)
		then fullimport currcidtree
		else do
			getContentIdentifierTree (Remote.uuid remote) >>= \case
				Nothing -> fullimport currcidtree
				Just prevcidtree -> candiffimport prevcidtree >>= \case
					Nothing -> fullimport currcidtree
					Just lastimportedtree -> diffimport cidtreemap prevcidtree currcidtree lastimportedtree
  where
	remember = recordContentIdentifierTree (Remote.uuid remote)

	-- In order to use a diff, the previous ContentIdentifier tree must
	-- not have been garbage collected. Which can happen since there
	-- are no git refs to it.
	--
	-- Also, a tree must have been imported before, and that tree must
	-- also have not been garbage collected (which is less likely to
	-- happen due to the remote tracking branch).
	candiffimport prevcidtree =
		catObjectMetaData prevcidtree >>= \case
			Nothing -> return Nothing
			Just _ -> getLastImportedTree remote >>= \case
				Nothing -> return Nothing
				Just lastimported@(LastImportedTree t) -> 
					ifM (isJust <$> catObjectMetaData t)
						( return (Just lastimported)
						, return Nothing
						)

	fullimport currcidtree = 
		importKeys remote importtreeconfig importcontent thirdpartypopulated importablecontents >>= \case
			ImportUnfinished -> return ImportUnfinished
			ImportFinished r -> do
				remember currcidtree
	 			return $ ImportFinished $ ImportedFull r
		
	diffimport cidtreemap prevcidtree currcidtree lastimportedtree = do
		(diff, cleanup) <- inRepo $ Git.DiffTree.diffTreeRecursive
			prevcidtree
			currcidtree
		let (removed, changed) = partition isremoval diff
		let mkicchanged ti = do
			v <- M.lookup (Git.DiffTree.dstsha ti) cidtreemap
			return (mkloc ti, v)
		let ic = ImportableContentsComplete $ ImportableContents
			{ importableContents = mapMaybe mkicchanged changed
			, importableHistory = []
			}
		importKeys remote importtreeconfig importcontent thirdpartypopulated ic >>= \case
			ImportUnfinished -> do
				void $ liftIO cleanup
				return ImportUnfinished
			ImportFinished (ImportableContentsComplete ic') -> 
				liftIO cleanup >>= \case
					False -> return ImportUnfinished
					True -> do
						remember currcidtree
						return $ ImportFinished $ 
							ImportedDiff lastimportedtree
								(mkdiff ic' removed)
			-- importKeys is not passed ImportableContentsChunked
			-- above, so it cannot return it
			ImportFinished (ImportableContentsChunked {}) -> error "internal"
		
	isremoval ti = Git.DiffTree.dstsha ti `elem` nullShas
	
	mkloc = mkImportLocation . getTopFilePath . Git.DiffTree.file

	mkdiff ic removed = ImportableContents
		{ importableContents = diffremoved ++ diffchanged
		, importableHistory = []
		}
	  where
		diffchanged = map
			(\(loc, v) -> (loc, DiffChanged v))
			(importableContents ic)
		diffremoved = map
			(\ti -> (mkloc ti, DiffRemoved))
			removed

{- Gets the tree that was last imported from the remote
 - (or exported to it if an export happened after the last import).
 -}
getLastImportedTree :: Remote -> Annex (Maybe LastImportedTree)
getLastImportedTree remote = do
	db <- Export.openDb (Remote.uuid remote)
	mtree <- liftIO $ Export.getExportTreeCurrent db
	Export.closeDb db
	return (LastImportedTree <$> mtree)

{- Downloads all new ContentIdentifiers, or when importcontent is False,
 - generates Keys without downloading.
 -
 - Generates either a Key or a git Sha, depending on annex.largefiles.
 - But when importcontent is False, it cannot generate a git Sha, 
 - so always generates Keys.
 -
 - Supports concurrency when enabled.
 -
 - Note that, when a ContentIdentifier has been imported before,
 - generates the same thing that was imported before, so annex.largefiles
 - is not reapplied.
 -}
importKeys
	:: Remote
	-> ImportTreeConfig
	-> Bool
	-> Bool
	-> ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)
	-> Annex (ImportResult (ImportableContentsChunkable Annex (Either Sha Key)))
importKeys remote importtreeconfig importcontent thirdpartypopulated importablecontents = do
	unless (canImportKeys remote importcontent) $
		giveup "This remote does not support importing without downloading content."
	-- This map is used to remember content identifiers that
	-- were just imported, before they have necessarily been
	-- stored in the database. This way, if the same content
	-- identifier appears multiple times in the
	-- importablecontents (eg when it has a history), 
	-- they will only be imported once.
	cidmap <- liftIO $ newTVarIO M.empty
	-- When concurrency is enabled, this set is needed to
	-- avoid two threads both importing the same content identifier.
	importing <- liftIO $ newTVarIO S.empty
	withciddb $ \db -> do
		db' <- CIDDb.needsUpdateFromLog db
			>>= maybe (pure db) (CIDDb.updateFromLog db)
		(prepclock (run cidmap importing db'))
  where
	-- When not importing content, reuse the same vector
	-- clock for all state that's recorded. This can save
	-- a little bit of disk space. Individual file downloads
	-- while downloading take too long for this optimisation
	-- to be safe to do.
	prepclock a
		| importcontent = a
		| otherwise = reuseVectorClockWhile a

	withciddb a = do
		cidlck <- calcRepo' gitAnnexContentIdentifierLock
		withExclusiveLock cidlck $
			bracket CIDDb.openDb CIDDb.closeDb a

	run cidmap importing db = do
		largematcher <- largeFilesMatcher
		case importablecontents of
			ImportableContentsComplete ic ->
				go False largematcher cidmap importing db ic >>= return . \case
					Nothing -> ImportUnfinished
					Just v -> ImportFinished $ ImportableContentsComplete v
			ImportableContentsChunked {} -> do
				c <- gochunked db (importableContentsChunk importablecontents)
				gohistory largematcher cidmap importing db (importableHistoryComplete importablecontents) >>= return . \case
					Nothing -> ImportUnfinished
					Just h -> ImportFinished $ ImportableContentsChunked
						{ importableContentsChunk = c
						, importableHistoryComplete = h
						}

	go oldversion largematcher cidmap importing db (ImportableContents l h) = do
		jobs <- forM l $ \i ->
			if thirdpartypopulated
				then Left <$> thirdpartypopulatedimport db i
				else startimport cidmap importing db i oldversion largematcher
		l' <- liftIO $ forM jobs $
			either pure (atomically . takeTMVar)
		if any isNothing l'
			then return Nothing
			else gohistory largematcher cidmap importing db h >>= return . \case
				Nothing -> Nothing
				Just h' -> Just $ ImportableContents (catMaybes l') h'
	
	gohistory largematcher cidmap importing db h = do
		h' <- mapM (go True largematcher cidmap importing db) h
		if any isNothing h'
			then return Nothing
			else return $ Just $ catMaybes h'
	
	gochunked db c
		-- Downloading cannot be done when chunked, since only
		-- the first chunk is processed before returning.
		| importcontent = giveup "importKeys does not support downloading chunked import"
		-- Chunked import is currently only used by thirdpartypopulated
		-- remotes.
		| not thirdpartypopulated = giveup "importKeys does not support chunked import when not thirdpartypopulated"
		| otherwise = do
			l <- forM (importableContentsSubTree c) $ \(loc, i) -> do
				let loc' = importableContentsChunkFullLocation (importableContentsSubDir c) loc
				thirdpartypopulatedimport db (loc', i) >>= return . \case
					Just (_loc, k) -> Just (loc, k)
					Nothing -> Nothing
			return $ ImportableContentsChunk
				{ importableContentsSubDir = importableContentsSubDir c
				, importableContentsSubTree = catMaybes l
				, importableContentsNextChunk =
					importableContentsNextChunk c >>= \case
						Nothing -> return Nothing
						Just c' -> withciddb $ \db' -> 
							prepclock $
								Just <$> gochunked db' c'
				}

	waitstart importing cid = liftIO $ atomically $ do
		s <- readTVar importing
		if S.member cid s
			then retry
			else writeTVar importing $ S.insert cid s
	
	signaldone importing cid = liftIO $ atomically $ do
		s <- readTVar importing
		writeTVar importing $ S.delete cid s
	
	startimport cidmap importing db i@(loc, (cid, _sz)) oldversion largematcher = getcidkey cidmap db cid >>= \case
		(k:ks) ->
			-- If the same content was imported before
			-- yielding multiple different keys, it's not clear
			-- which is best to use this time, so pick the
			-- first in the list. But, if any of them is a
			-- git sha, use it, because the content must
			-- be included in the git repo then.
			let v = case mapMaybe keyGitSha (k:ks) of
				(sha:_) -> Left sha
				[] -> Right k
			in return $ Left $ Just (loc, v)
		[] -> do
			job <- liftIO $ newEmptyTMVarIO
			let ai = ActionItemOther (Just (QuotedPath (fromImportLocation loc)))
			let si = SeekInput []
			let importaction = starting ("import " ++ Remote.name remote) ai si $ do
				when oldversion $
					showNote "old version"
				tryNonAsync (importordownload cidmap i largematcher) >>= \case
					Left e -> next $ do
						warning (UnquotedString (show e))
						liftIO $ atomically $
							putTMVar job Nothing
						return False
					Right r -> next $ do
						liftIO $ atomically $
							putTMVar job r
						return True
			commandAction $ bracket_
				(waitstart importing cid)
				(signaldone importing cid)
				importaction
			return (Right job)
	
	thirdpartypopulatedimport db (loc, (cid, sz)) = 
		case Remote.importKey ia of
			Nothing -> return Nothing
			Just importkey ->
				tryNonAsync (importkey loc cid sz nullMeterUpdate) >>= \case
					Right (Just k) -> do
						recordcidkeyindb db cid k
						logChange NoLiveUpdate k (Remote.uuid remote) InfoPresent
						return $ Just (loc, Right k)
					Right Nothing -> return Nothing
					Left e -> do
						warning (UnquotedString (show e))
						return Nothing
	
	importordownload cidmap (loc, (cid, sz)) largematcher = do
		f <- locworktreefile loc
		matcher <- largematcher f
		-- When importing a key is supported, always use it rather
		-- than downloading and retrieving a key, to avoid
		-- generating trees with different keys for the same content.
		let act = if importcontent
			then case Remote.importKey ia of
				Nothing -> dodownload
				Just _ -> if Utility.Matcher.introspect matchNeedsFileContent (fst matcher)
					then dodownload
					else doimport
			else doimport
		act cidmap (loc, (cid, sz)) f matcher

	doimport cidmap (loc, (cid, sz)) f matcher =
		case Remote.importKey ia of
			Nothing -> error "internal" -- checked earlier
			Just importkey -> do
				when (Utility.Matcher.introspect matchNeedsFileContent (fst matcher)) $
					giveup "annex.largefiles configuration examines file contents, so cannot import without content."
 				let mi = MatchingInfo ProvidedInfo
					{ providedFilePath = Just f
					, providedKey = Nothing
					, providedFileSize = Just sz
					, providedMimeType = Nothing
					, providedMimeEncoding = Nothing
					, providedLinkType = Nothing
					}
				islargefile <- checkMatcher' matcher mi NoLiveUpdate mempty
				metered Nothing sz bwlimit $ const $ if islargefile
					then doimportlarge importkey cidmap loc cid sz f
					else doimportsmall cidmap loc cid sz
	
	doimportlarge importkey cidmap loc cid sz f p =
		tryNonAsync importer >>= \case
			Right (Just (k, True)) -> return $ Just (loc, Right k)
			Right _ -> return Nothing
			Left e -> do
				warning (UnquotedString (show e))
				return Nothing
	  where
		importer = do
			-- Don't display progress when generating
			-- key, if the content will later be
			-- downloaded, which is a more expensive
			-- operation generally.
			let p' = if importcontent then nullMeterUpdate else p
			importkey loc cid sz p' >>= \case
				Nothing -> return Nothing
				Just k -> checkSecureHashes k >>= \case
					Nothing -> do
						recordcidkey cidmap cid k
						logChange NoLiveUpdate k (Remote.uuid remote) InfoPresent
						if importcontent
							then getcontent k
							else return (Just (k, True))
					Just msg -> giveup (msg ++ " to import")

		getcontent :: Key -> Annex (Maybe (Key, Bool))
		getcontent k = do
			let af = AssociatedFile (Just f)
			let downloader p' tmpfile = do
				_ <- Remote.retrieveExportWithContentIdentifier
					ia loc [cid] tmpfile
					(Left k)
					(combineMeterUpdate p' p)
				ok <- moveAnnex k tmpfile
				when ok $
					logStatus NoLiveUpdate k InfoPresent
				return (Just (k, ok))
			checkDiskSpaceToGet k Nothing Nothing $
				notifyTransfer Download af $
					download' (Remote.uuid remote) k af Nothing stdRetry $ \p' ->
						withTmp k $ downloader p'
			
	-- The file is small, so is added to git, so while importing
	-- without content does not retrieve annexed files, it does
	-- need to retrieve this file.
	doimportsmall cidmap loc cid sz p = do
		let downloader tmpfile = do
			(k, _) <- Remote.retrieveExportWithContentIdentifier
				ia loc [cid] tmpfile
				(Right (mkkey tmpfile))
				p
			case keyGitSha k of
				Just sha -> do
					recordcidkey cidmap cid k
					return sha
				Nothing -> error "internal"
		checkDiskSpaceToGet tmpkey Nothing Nothing $
			withTmp tmpkey $ \tmpfile ->
				tryNonAsync (downloader tmpfile) >>= \case
					Right sha -> return $ Just (loc, Left sha)
					Left e -> do
						warning (UnquotedString (show e))
						return Nothing
	  where
		tmpkey = importKey cid sz
		mkkey tmpfile = gitShaKey <$> hashFile tmpfile
	
	dodownload cidmap (loc, (cid, sz)) f matcher = do
		let af = AssociatedFile (Just f)
		let downloader tmpfile p = do
			(k, _) <- Remote.retrieveExportWithContentIdentifier
				ia loc [cid] tmpfile
				(Right (mkkey tmpfile))
				p
			case keyGitSha k of
				Nothing -> do
					ok <- moveAnnex k tmpfile
					when ok $ do
						recordcidkey cidmap cid k
						logStatus NoLiveUpdate k InfoPresent
						logChange NoLiveUpdate k (Remote.uuid remote) InfoPresent
					return (Right k, ok)
				Just sha -> do
					recordcidkey cidmap cid k
					return (Left sha, True)
		let rundownload tmpfile p = tryNonAsync (downloader tmpfile p) >>= \case
			Right (v, True) -> return $ Just (loc, v)
			Right (_, False) -> return Nothing
			Left e -> do
				warning (UnquotedString (show e))
				return Nothing
		checkDiskSpaceToGet tmpkey Nothing Nothing $
			notifyTransfer Download af $
				download' (Remote.uuid remote) tmpkey af Nothing stdRetry $ \p ->
					withTmp tmpkey $ \tmpfile ->
						metered (Just p) tmpkey bwlimit $
							const (rundownload tmpfile)
	  where
		tmpkey = importKey cid sz
	
		mkkey tmpfile = do
			let mi = MatchingFile FileInfo
				{ matchFile = f
				, contentFile = tmpfile
				, matchKey = Nothing
				}
			islargefile <- checkMatcher' matcher mi NoLiveUpdate mempty
			if islargefile
				then do
					backend <- chooseBackend f
					let ks = KeySource
						{ keyFilename = f
						, contentLocation = tmpfile
						, inodeCache = Nothing
						}
					fst <$> genKey ks nullMeterUpdate backend
				else gitShaKey <$> hashFile tmpfile
	
	ia = Remote.importActions remote
				
	bwlimit = remoteAnnexBwLimitDownload (Remote.gitconfig remote)
			<|> remoteAnnexBwLimit (Remote.gitconfig remote)

	locworktreefile loc = fromRepo $ fromTopFilePath $ asTopFilePath $
		case importtreeconfig of
			ImportTree -> fromImportLocation loc
			ImportSubTree subdir _ ->
				getTopFilePath subdir </> fromImportLocation loc

	getcidkey cidmap db cid = liftIO $
		-- Avoiding querying the database when it's empty speeds up
		-- the initial import.
		if CIDDb.databaseIsEmpty db
			then getcidkeymap cidmap cid
			else CIDDb.getContentIdentifierKeys db rs cid >>= \case
				[] -> getcidkeymap cidmap cid
				l -> return l

	getcidkeymap cidmap cid =
		atomically $ maybeToList . M.lookup cid <$> readTVar cidmap

	recordcidkey cidmap cid k = do
		liftIO $ atomically $ modifyTVar' cidmap $
			M.insert cid k
		-- Only record in log now; the database will be updated
		-- later from the log, and the cidmap will be used for now.
		recordcidkeyinlog cid k
	
	recordcidkeyindb db cid k = do
		liftIO $ CIDDb.recordContentIdentifier db rs cid k
		recordcidkeyinlog cid k
	
	recordcidkeyinlog cid k =
		CIDLog.recordContentIdentifier rs cid k

	rs = Remote.remoteStateHandle remote

{- Temporary key used for import of a ContentIdentifier while downloading
 - content, before generating its real key. -}
importKey :: ContentIdentifier -> Integer -> Key
importKey (ContentIdentifier cid) size = mkKey $ \k -> k
	{ keyName = genKeyName (decodeBS cid)
	, keyVariety = OtherKey "CID"
	, keySize = Just size
	}

{-- Export omits non-preferred content from the tree stored on the
 -- remote. So the import will normally have that content
 -- omitted (unless something else added files with the same names to the
 -- special remote).
 --
 -- That presents a problem: Merging the imported tree would result
 -- in deletion of the files that were excluded from export.
 -- To avoid that happening, this adds them back to the imported tree.
 --}
addBackExportExcluded :: Remote -> Sha -> Annex Sha
addBackExportExcluded remote importtree =
	getExportExcluded (Remote.uuid remote) >>= \case
		[] -> return importtree
		excludedlist -> inRepo $
			adjustTree
				-- don't remove any
				(pure . Just)
				excludedlist
				-- if something was imported with the same
				-- name as a file that was previously
				-- excluded from import, use what was imported
				(\imported _excluded -> imported)
				[]
				importtree

{- Match the preferred content of the remote at import time.
 -
 - Only keyless tokens are supported, because the keys are not known
 - until an imported file is downloaded, which is too late to bother
 - excluding it from an import. So prunes any tokens in the preferred
 - content expression that need keys.
 -}
makeImportMatcher :: Remote -> Annex (Either String (FileMatcher Annex))
makeImportMatcher r = load preferredContentTokens >>= \case
	Nothing -> return $ Right (matchAll, matcherdesc)
	Just (Right v) -> return $ Right (v, matcherdesc)
	Just (Left err) -> return $ Left err
  where
	load t = M.lookup (Remote.uuid r) . fst
		<$> preferredRequiredMapsLoad' pruneImportMatcher t
	matcherdesc = MatcherDesc "preferred content"

pruneImportMatcher :: Utility.Matcher.Matcher (MatchFiles a) -> Utility.Matcher.Matcher (MatchFiles a)
pruneImportMatcher = Utility.Matcher.pruneMatcher matchNeedsKey

{- Gets the ImportableContents from the remote.
 -
 - Filters out any paths that include a ".git" component, because git does
 - not allow storing ".git" in a git repository. While it is possible to
 - write a git tree that contains that, git will complain and refuse to
 - check it out.
 -
 - Filters out new things not matching the FileMatcher or that are
 - gitignored. However, files that are already in git get imported
 - regardless. (Similar to how git add behaves on gitignored files.)
 - This avoids creating a remote tracking branch that, when merged,
 - would delete the files.
 -
 - Throws exception if unable to contact the remote.
 - Returns Nothing when there is no change since last time.
 -}
getImportableContents :: Remote -> ImportTreeConfig -> CheckGitIgnore -> FileMatcher Annex -> Annex (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)))
getImportableContents r importtreeconfig ci matcher = do
	Remote.listImportableContents (Remote.importActions r) >>= \case
		Just (ImportableContentsComplete ic) -> do
			dbhandle <- opendbhandle
			Just . ImportableContentsComplete
				<$> filterunwanted dbhandle ic
		Just (c@(ImportableContentsChunked {})) -> do
			dbhandle <- opendbhandle
			Just <$> filterunwantedchunked dbhandle c
		Nothing -> return Nothing
  where
	filterunwanted dbhandle ic = ImportableContents
		<$> filterM (wanted dbhandle) (importableContents ic)
		<*> mapM (filterunwanted dbhandle) (importableHistory ic)
	
	filterunwantedchunked dbhandle c = ImportableContentsChunked
		<$> filterunwantedchunk dbhandle (importableContentsChunk c)
		<*> mapM (filterunwanted dbhandle) (importableHistoryComplete c)

	filterunwantedchunk dbhandle c = ImportableContentsChunk
		<$> pure (importableContentsSubDir c)
		<*> filterM (wantedunder dbhandle (importableContentsSubDir c))
			(importableContentsSubTree c)
		<*> pure (
			importableContentsNextChunk c >>= \case
				Nothing -> return Nothing
				Just c' -> Just <$> filterunwantedchunk dbhandle c'
			)

	opendbhandle = do
		h <- Export.openDb (Remote.uuid r)
		void $ Export.updateExportTreeFromLog h
		return h

	wanted dbhandle (loc, (_cid, sz))
		| ingitdir = pure False
		| otherwise =
			isknown <||> (matches <&&> notignored)
	  where
		-- Checks, from least to most expensive.
#ifdef mingw32_HOST_OS
		ingitdir = ".git" `elem` Posix.splitDirectories (fromOsPath (fromImportLocation loc))
#else
		ingitdir = literalOsPath ".git" `elem` splitDirectories (fromImportLocation loc)
#endif
		matches = matchesImportLocation matcher loc sz
		isknown = isKnownImportLocation dbhandle loc
		notignored = notIgnoredImportLocation importtreeconfig ci loc
	
	wantedunder dbhandle root (loc, v) = 
		wanted dbhandle (importableContentsChunkFullLocation root loc, v)

isKnownImportLocation :: Export.ExportHandle -> ImportLocation -> Annex Bool
isKnownImportLocation dbhandle loc = liftIO $
	not . null <$> Export.getExportTreeKey dbhandle loc

matchesImportLocation :: FileMatcher Annex -> ImportLocation -> Integer -> Annex Bool
matchesImportLocation matcher loc sz = checkMatcher' matcher mi NoLiveUpdate mempty
  where
	mi = MatchingInfo $ ProvidedInfo
		{ providedFilePath = Just (fromImportLocation loc)
		, providedKey = Nothing
		, providedFileSize = Just sz
		, providedMimeType = Nothing
		, providedMimeEncoding = Nothing
		, providedLinkType = Nothing
		}

notIgnoredImportLocation :: ImportTreeConfig -> CheckGitIgnore -> ImportLocation -> Annex Bool
notIgnoredImportLocation importtreeconfig ci loc = not <$> checkIgnored ci f
  where
	f = case importtreeconfig of
		ImportSubTree dir _ ->
			getTopFilePath dir </> fromImportLocation loc
		ImportTree ->
			fromImportLocation loc
