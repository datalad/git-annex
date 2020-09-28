{- git-annex import from remotes
 -
 - Copyright 2019-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Import (
	ImportTreeConfig(..),
	ImportCommitConfig(..),
	buildImportCommit,
	buildImportTrees,
	importKeys,
	filterImportableContents,
	makeImportMatcher,
	listImportableContents,
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
import Annex.HashObject
import Annex.Transfer
import Command
import Backend
import Types.Key
import Types.KeySource
import Messages.Progress
import Utility.DataUnits
import Utility.Metered
import Logs.Export
import Logs.Location
import Logs.PreferredContent
import Types.FileMatcher
import Annex.FileMatcher
import Utility.Matcher (isEmpty)
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as CIDDb
import qualified Logs.ContentIdentifier as CIDLog
import Backend.Utilities

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.ByteString as P

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
	-> ImportableContents (Either Sha Key)
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
		buildImportCommit' remote importcommitconfig trackingcommit imported >>= \case
			Just finalcommit -> do
				updatestate finaltree
				return (Just finalcommit)
			Nothing -> return Nothing
	
	updatestate committedtree = do
		importedtree <- case subdir of
			Nothing -> pure committedtree
			Just dir -> 
				let subtreeref = Ref $
					fromRef' committedtree 
						<> ":"
						<> getTopFilePath dir
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
		| otherwise = do
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
	  where
		h'@(History t s) = mapHistory historyCommitTree h

	importeddepth = historyDepth imported

	sametodepth b = imported == truncateHistoryToDepth importeddepth b

	mkcommit parents tree = inRepo $ Git.Branch.commitTree
		(importCommitMode importcommitconfig)
		(importCommitMessage importcommitconfig)
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

{- Builds a history of git trees reflecting the ImportableContents.
 -
 - When a subdir is provided, imported tree is grafted into the basetree at
 - that location, replacing any object that was there.
 -}
buildImportTrees
	:: Ref
	-> Maybe TopFilePath
	-> ImportableContents (Either Sha Key)
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
	
	mktreeitem (loc, v) = case v of
		Right k -> do
			relf <- fromRepo $ fromTopFilePath topf
			symlink <- calcRepo $ gitAnnexLink (fromRawFilePath relf) k
			linksha <- hashSymlink symlink
			return $ TreeItem treepath (fromTreeItemType TreeSymlink) linksha
		Left sha -> 
			return $ TreeItem treepath (fromTreeItemType TreeFile) sha
	  where
		lf = fromImportLocation loc
		treepath = asTopFilePath lf
		topf = asTopFilePath $
			maybe lf (\sd -> getTopFilePath sd P.</> lf) msubdir

{- Downloads all new ContentIdentifiers, or when importcontent is False,
 - generates Keys without downloading.
 -
 - Generates either a Key or a git Sha, depending on annex.largefiles.
 - But when importcontent is False, it cannot match on annex.largefiles
 - (or generate a git Sha), so always generates Keys.
 -
 - Supports concurrency when enabled.
 -
 - If it fails on any file, the whole thing fails with Nothing, 
 - but it will resume where it left off.
 -
 - Note that, when a ContentIdentifier has been imported before,
 - generates the same thing that was imported before, so annex.largefiles
 - is not reapplied.
 -}
importKeys
	:: Remote
	-> ImportTreeConfig
	-> Bool
	-> ImportableContents (ContentIdentifier, ByteSize)
	-> Annex (Maybe (ImportableContents (Either Sha Key)))
importKeys remote importtreeconfig importcontent importablecontents = do
	when (not importcontent && isNothing (Remote.importKey ia)) $
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
	withExclusiveLock gitAnnexContentIdentifierLock $
		bracket CIDDb.openDb CIDDb.closeDb $ \db -> do
			CIDDb.needsUpdateFromLog db
				>>= maybe noop (CIDDb.updateFromLog db)
			go False cidmap importing importablecontents db
  where
	go oldversion cidmap importing (ImportableContents l h) db = do
		largematcher <- largeFilesMatcher
		jobs <- forM l $ \i ->
			startimport cidmap importing db i oldversion largematcher
		l' <- liftIO $ forM jobs $
			either pure (atomically . takeTMVar)
		if any isNothing l'
			then return Nothing
			else do
				h' <- mapM (\ic -> go True cidmap importing ic db) h
				if any isNothing h'
					then return Nothing
					else return $ Just $
						ImportableContents
							(catMaybes l')
							(catMaybes h')
	
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
			-- yeilding multiple different keys, it's not clear
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
			let ai = ActionItemOther (Just (fromRawFilePath (fromImportLocation loc)))
			let si = SeekInput []
			let importaction = starting ("import " ++ Remote.name remote) ai si $ do
				when oldversion $
					showNote "old version"
				tryNonAsync (importordownload cidmap db i largematcher) >>= \case
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
				(waitstart importing cid)
				(signaldone importing cid)
				importaction
			return (Right job)
	
	importordownload
		| not importcontent = doimport
		| otherwise = dodownload

	doimport cidmap db (loc, (cid, sz)) _largematcher =
		case Remote.importKey ia of
			Nothing -> error "internal" -- checked earlier
			Just a -> do
				let importer p = do
					unsizedk <- a loc cid p
					-- This avoids every remote needing
					-- to add the size.
					let k = alterKey unsizedk $ \kd -> kd
						{ keySize = keySize kd <|> Just sz }
					checkSecureHashes k >>= \case
						Nothing -> do
							recordcidkey cidmap db cid k
							logChange k (Remote.uuid remote) InfoPresent
							return (Right k)
						Just msg -> giveup (msg ++ " to import")
				let runimport p = tryNonAsync (importer p) >>= \case
					Right k -> return $ Just (loc, k)
					Left e -> do
						warning (show e)
						return Nothing
				metered Nothing	sz $
					const runimport
	
	dodownload cidmap db (loc, (cid, sz)) largematcher = do
		f <- locworktreefile loc
		let af = AssociatedFile (Just f)
		let downloader tmpfile p = do
			k <- Remote.retrieveExportWithContentIdentifier
				ia loc cid tmpfile 
				(mkkey f tmpfile largematcher)
				p
			case keyGitSha k of
				Nothing -> do
					ok <- moveAnnex k tmpfile
					when ok $ do
						recordcidkey cidmap db cid k
						logStatus k InfoPresent
						logChange k (Remote.uuid remote) InfoPresent
					return (Right k, ok)
				Just sha -> do
					recordcidkey cidmap db cid k
					return (Left sha, True)
		let rundownload tmpfile p = tryNonAsync (downloader tmpfile p) >>= \case
			Right (v, True) -> return $ Just (loc, v)
			Right (_, False) -> return Nothing
			Left e -> do
				warning (show e)
				return Nothing
		checkDiskSpaceToGet tmpkey Nothing $
			notifyTransfer Download af $
				download (Remote.uuid remote) tmpkey af stdRetry $ \p ->
					withTmp tmpkey $ \tmpfile ->
						metered (Just p) tmpkey $
							const (rundownload tmpfile)
	  where
		tmpkey = importKey cid sz
	
	ia = Remote.importActions remote
	
	mkkey f tmpfile largematcher = do
		matcher <- largematcher (fromRawFilePath f)
		let mi = MatchingFile FileInfo
			{ matchFile = f
			, contentFile = Just (toRawFilePath tmpfile)
			}
		islargefile <- checkMatcher' matcher mi mempty
		if islargefile
			then do
				backend <- chooseBackend (fromRawFilePath f)
				let ks = KeySource
					{ keyFilename = f
					, contentLocation = toRawFilePath tmpfile
					, inodeCache = Nothing
					}
				fst <$> genKey ks nullMeterUpdate backend
			else gitShaKey <$> hashFile tmpfile

	locworktreefile loc = fromRepo $ fromTopFilePath $ asTopFilePath $
		case importtreeconfig of
			ImportTree -> fromImportLocation loc
			ImportSubTree subdir _ ->
				getTopFilePath subdir P.</> fromImportLocation loc

	getcidkey cidmap db cid = liftIO $
		CIDDb.getContentIdentifierKeys db rs cid >>= \case
			[] -> atomically $
				maybeToList . M.lookup cid <$> readTVar cidmap
			l -> return l

	recordcidkey cidmap db cid k = do
		liftIO $ atomically $ modifyTVar' cidmap $
			M.insert cid k
		liftIO $ CIDDb.recordContentIdentifier db rs cid k
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
 - excluding it from an import.
 -}
makeImportMatcher :: Remote -> Annex (Either String (FileMatcher Annex))
makeImportMatcher r = load preferredContentKeylessTokens >>= \case
	Nothing -> return $ Right matchAll
	Just (Right v) -> return $ Right v
	Just (Left err) -> load preferredContentTokens >>= \case
		Just (Left err') -> return $ Left err'
		_ -> return $ Left $
			"The preferred content expression contains terms that cannot be checked when importing: " ++ err
  where
	load t = M.lookup (Remote.uuid r) . fst <$> preferredRequiredMapsLoad' t

wantImport :: FileMatcher Annex -> ImportLocation -> ByteSize -> Annex Bool
wantImport matcher loc sz = checkMatcher' matcher mi mempty
  where
	mi = MatchingInfo $ ProvidedInfo
		{ providedFilePath = Right $ fromRawFilePath $ fromImportLocation loc
		, providedKey = unavail "key"
		, providedFileSize = Right sz
		, providedMimeType = unavail "mime"
		, providedMimeEncoding = unavail "mime"
		}
	-- This should never run, as long as the FileMatcher was generated
	-- using the preferredContentKeylessTokens.
	unavail v = Left $ error $ "Internal error: unavailable " ++ v

{- If a file is not preferred content, but it was previously exported or
 - imported to the remote, not importing it would result in a remote
 - tracking branch that, when merged, would delete the file.
 -
 - To avoid that problem, such files are included in the import.
 - The next export will remove them from the remote.
 -}
shouldImport :: Export.ExportHandle -> FileMatcher Annex -> ImportLocation -> ByteSize -> Annex Bool
shouldImport dbhandle matcher loc sz = 
	wantImport matcher loc sz
		<||>
	liftIO (not . null <$> Export.getExportTreeKey dbhandle loc)

filterImportableContents :: Remote -> FileMatcher Annex -> ImportableContents (ContentIdentifier, ByteSize) -> Annex (ImportableContents (ContentIdentifier, ByteSize))
filterImportableContents r matcher importable
	| isEmpty matcher = return importable
	| otherwise = do
		dbhandle <- Export.openDb (Remote.uuid r)
		go dbhandle importable
  where
	go dbhandle ic = ImportableContents
		<$> filterM (match dbhandle) (importableContents ic)
		<*> mapM (go dbhandle) (importableHistory ic)
	
	match dbhandle (loc, (_cid, sz)) = shouldImport dbhandle matcher loc sz
	
{- Gets the ImportableContents from the remote.
 -
 - Filters out any paths that include a ".git" component, because git does
 - not allow storing ".git" in a git repository. While it is possible to
 - write a git tree that contains that, git will complain and refuse to
 - check it out.
 -}
listImportableContents :: Remote -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
listImportableContents r = fmap removegitspecial
	<$> Remote.listImportableContents (Remote.importActions r)
  where
	removegitspecial ic = ImportableContents
		{ importableContents = 
			filter (not . gitspecial . fst) (importableContents ic)
		, importableHistory =
			map removegitspecial (importableHistory ic)
		}
	gitspecial l = ".git" `elem` Posix.splitDirectories (fromRawFilePath (fromImportLocation l))
