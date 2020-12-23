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
	recordImportTree,
	canImportKeys,
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
import Annex.CheckIgnore
import Annex.VectorClock
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
import qualified Utility.Matcher
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as CIDDb
import qualified Logs.ContentIdentifier as CIDLog
import Backend.Utilities

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified System.FilePath.Posix.ByteString as Posix
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
	go trackingcommit = do
		(imported, updatestate) <- recordImportTree remote importtreeconfig importable
		buildImportCommit' remote importcommitconfig trackingcommit imported >>= \case
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
	-> ImportableContents (Either Sha Key)
	-> Annex (History Sha, Annex ())
recordImportTree remote importtreeconfig importable = do
	imported@(History finaltree _) <- buildImportTrees basetree subdir importable
	return (imported, updatestate finaltree)
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
		recordExport (Remote.uuid remote) importedtree $ ExportChange
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
			symlink <- calcRepo $ gitAnnexLink relf k
			linksha <- hashSymlink symlink
			return $ TreeItem treepath (fromTreeItemType TreeSymlink) linksha
		Left sha -> 
			return $ TreeItem treepath (fromTreeItemType TreeFile) sha
	  where
		lf = fromImportLocation loc
		treepath = asTopFilePath lf
		topf = asTopFilePath $
			maybe lf (\sd -> getTopFilePath sd P.</> lf) msubdir

canImportKeys :: Remote -> Bool -> Bool
canImportKeys remote importcontent =
	importcontent || isJust (Remote.importKey ia)
  where
	ia = Remote.importActions remote

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
	-> Bool
	-> ImportableContents (ContentIdentifier, ByteSize)
	-> Annex (Maybe (ImportableContents (Either Sha Key)))
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
	withExclusiveLock gitAnnexContentIdentifierLock $
		bracket CIDDb.openDb CIDDb.closeDb $ \db -> do
			CIDDb.needsUpdateFromLog db
				>>= maybe noop (CIDDb.updateFromLog db)
			(run (go False cidmap importing importablecontents db))
  where
	-- When not importing content, reuse the same vector
	-- clock for all state that's recorded. This can save
	-- a little bit of disk space. Individual file downloads
	-- while downloading take too long for this optimisation
	-- to be safe to do.
	run a
		| importcontent = a
		| otherwise = reuseVectorClockWhile a

	go oldversion cidmap importing (ImportableContents l h) db = do
		largematcher <- largeFilesMatcher
		jobs <- forM l $ \i ->
			if thirdpartypopulated
				then thirdpartypopulatedimport cidmap db i
				else startimport cidmap importing db i oldversion largematcher
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
	
	thirdpartypopulatedimport cidmap db (loc, (cid, sz)) = 
		case Remote.importKey ia of
			Nothing -> return $ Left Nothing
			Just importkey ->
				tryNonAsync (importkey loc cid sz nullMeterUpdate) >>= \case
					Right (Just k) -> do
						recordcidkey cidmap db cid k
						logChange k (Remote.uuid remote) InfoPresent				
						return $ Left $ Just (loc, Right k)
					Right Nothing -> return $ Left Nothing
					Left e -> do
						warning (show e)
						return $ Left Nothing
	
	importordownload cidmap db (loc, (cid, sz)) largematcher= do
		f <- locworktreefile loc
		matcher <- largematcher f
		-- When importing a key is supported, always use it rather
		-- than downloading and retrieving a key, to avoid
		-- generating trees with different keys for the same content.
		let act = if importcontent
			then case Remote.importKey ia of
				Nothing -> dodownload
				Just _ -> if Utility.Matcher.introspect matchNeedsFileContent matcher
					then dodownload
					else doimport
			else doimport
		act cidmap db (loc, (cid, sz)) f matcher

	doimport cidmap db (loc, (cid, sz)) f matcher =
		case Remote.importKey ia of
			Nothing -> error "internal" -- checked earlier
			Just importkey -> do
				when (Utility.Matcher.introspect matchNeedsFileContent matcher) $
					giveup "annex.largefiles configuration examines file contents, so cannot import without content."
 				let mi = MatchingInfo ProvidedInfo
					{ providedFilePath = f
					, providedKey = Nothing
					, providedFileSize = sz
					, providedMimeType = Nothing
					, providedMimeEncoding = Nothing
					}
				islargefile <- checkMatcher' matcher mi mempty
				metered Nothing sz $ const $ if islargefile
					then doimportlarge importkey cidmap db loc cid sz f
					else doimportsmall cidmap db loc cid sz
	
	doimportlarge importkey cidmap db loc cid sz f p =
		tryNonAsync importer >>= \case
			Right (Just (k, True)) -> return $ Just (loc, Right k)
			Right _ -> return Nothing
			Left e -> do
				warning (show e)
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
						recordcidkey cidmap db cid k
						logChange k (Remote.uuid remote) InfoPresent
						if importcontent
							then getcontent k
							else return (Just (k, True))
					Just msg -> giveup (msg ++ " to import")

		getcontent :: Key -> Annex (Maybe (Key, Bool))
		getcontent k = do
			let af = AssociatedFile (Just f)
			let downloader p' tmpfile = do
				k' <- Remote.retrieveExportWithContentIdentifier
					ia loc cid (fromRawFilePath tmpfile)
					(pure k)
					(combineMeterUpdate p' p)
				ok <- moveAnnex k' af tmpfile
				when ok $
					logStatus k InfoPresent
				return (Just (k', ok))
			checkDiskSpaceToGet k Nothing $
				notifyTransfer Download af $
					download' (Remote.uuid remote) k af stdRetry $ \p' ->
						withTmp k $ downloader p'
			
	-- The file is small, so is added to git, so while importing
	-- without content does not retrieve annexed files, it does
	-- need to retrieve this file.
	doimportsmall cidmap db loc cid sz p = do
		let downloader tmpfile = do
			k <- Remote.retrieveExportWithContentIdentifier
				ia loc cid (fromRawFilePath tmpfile)
				(mkkey tmpfile)
				p
			case keyGitSha k of
				Just sha -> do
					recordcidkey cidmap db cid k
					return sha
				Nothing -> error "internal"
		checkDiskSpaceToGet tmpkey Nothing $
			withTmp tmpkey $ \tmpfile ->
				tryNonAsync (downloader tmpfile) >>= \case
					Right sha -> return $ Just (loc, Left sha)
					Left e -> do
						warning (show e)
						return Nothing
	  where
		tmpkey = importKey cid sz
		mkkey tmpfile = gitShaKey <$> hashFile tmpfile
	
	dodownload cidmap db (loc, (cid, sz)) f matcher = do
		let af = AssociatedFile (Just f)
		let downloader tmpfile p = do
			k <- Remote.retrieveExportWithContentIdentifier
				ia loc cid (fromRawFilePath tmpfile)
				(mkkey tmpfile)
				p
			case keyGitSha k of
				Nothing -> do
					ok <- moveAnnex k af tmpfile
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
				download' (Remote.uuid remote) tmpkey af stdRetry $ \p ->
					withTmp tmpkey $ \tmpfile ->
						metered (Just p) tmpkey $
							const (rundownload tmpfile)
	  where
		tmpkey = importKey cid sz
	
		mkkey tmpfile = do
			let mi = MatchingFile FileInfo
				{ matchFile = f
				, contentFile = Just tmpfile
				, matchKey = Nothing
				}
			islargefile <- checkMatcher' matcher mi mempty
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
getImportableContents :: Remote -> ImportTreeConfig -> CheckGitIgnore -> FileMatcher Annex -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
getImportableContents r importtreeconfig ci matcher = do
	Remote.listImportableContents (Remote.importActions r) >>= \case
		Just importable -> do
			dbhandle <- Export.openDb (Remote.uuid r)
			Just <$> filterunwanted dbhandle importable
		Nothing -> return Nothing
  where
	filterunwanted dbhandle ic = ImportableContents
		<$> filterM (wanted dbhandle) (importableContents ic)
		<*> mapM (filterunwanted dbhandle) (importableHistory ic)
	
	wanted dbhandle (loc, (_cid, sz))
		| ingitdir = pure False
		| otherwise =
			isknown <||> (matches <&&> notignored)
	  where
		-- Checks, from least to most expensive.
		ingitdir = ".git" `elem` Posix.splitDirectories (fromImportLocation loc)
		matches = matchesImportLocation matcher loc sz
		isknown = isKnownImportLocation dbhandle loc
		notignored = notIgnoredImportLocation importtreeconfig ci loc

isKnownImportLocation :: Export.ExportHandle -> ImportLocation -> Annex Bool
isKnownImportLocation dbhandle loc = liftIO $
	not . null <$> Export.getExportTreeKey dbhandle loc

matchesImportLocation :: FileMatcher Annex -> ImportLocation -> Integer -> Annex Bool
matchesImportLocation matcher loc sz = checkMatcher' matcher mi mempty
  where
	mi = MatchingInfo $ ProvidedInfo
		{ providedFilePath = fromImportLocation loc
		, providedKey = Nothing
		, providedFileSize = sz
		, providedMimeType = Nothing
		, providedMimeEncoding = Nothing
		}

notIgnoredImportLocation :: ImportTreeConfig -> CheckGitIgnore -> ImportLocation -> Annex Bool
notIgnoredImportLocation importtreeconfig ci loc = not <$> checkIgnored ci f
  where
	f = case importtreeconfig of
		ImportSubTree dir _ ->
			getTopFilePath dir P.</> fromImportLocation loc
		ImportTree ->
			fromImportLocation loc
