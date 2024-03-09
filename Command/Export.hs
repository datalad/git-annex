{- git-annex command
 -
 - Copyright 2017-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Export where

import Command
import qualified Annex
import qualified Git
import qualified Git.DiffTree
import qualified Git.LsTree
import qualified Git.Tree
import qualified Git.Ref
import Git.Types
import Git.FilePath
import Git.Sha
import qualified Remote
import Types.Remote
import Types.Export
import Annex.Export
import Annex.Content
import Annex.Transfer
import Annex.CatFile
import Annex.FileMatcher
import Annex.RemoteTrackingBranch
import Logs.Location
import Logs.Export
import Logs.PreferredContent
import Database.Export
import Config
import Utility.Tmp
import Utility.Metered
import Utility.Matcher

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Concurrent

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption] $
	command "export" SectionCommon
		"export a tree of files to a special remote"
		paramTreeish (seek <$$> optParser)

data ExportOptions = ExportOptions
	{ exportTreeish :: Git.Ref
	-- ^ can be a tree, a branch, a commit, or a tag
	, exportRemote :: DeferredParse Remote
	, exportTracking :: Bool
	}

optParser :: CmdParamsDesc -> Parser ExportOptions
optParser _ = ExportOptions
	<$> (Git.Ref <$> parsetreeish)
	<*> (mkParseRemoteOption <$> parseToOption)
	<*> parsetracking
  where
	parsetreeish = argument str
		( metavar paramTreeish
		)
	parsetracking = switch
		( long "tracking"
		<> help ("track changes to the " ++ paramTreeish ++ " (deprecated)")
		)

-- To handle renames which swap files, the exported file is first renamed
-- to a stable temporary name based on the key.
exportTempName :: Key -> ExportLocation
exportTempName ek = mkExportLocation $ toRawFilePath $
	".git-annex-tmp-content-" ++ serializeKey ek

seek :: ExportOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	r <- getParsed (exportRemote o)
	unlessM (isExportSupported r) $
		giveup "That remote does not support exports."
	
	-- handle deprecated option
	when (exportTracking o) $
		setConfig (remoteAnnexConfig r "tracking-branch")
			(fromRef $ exportTreeish o)
	
	tree <- filterExport r =<<
		fromMaybe (giveup "unknown tree") <$>
		inRepo (Git.Ref.tree (exportTreeish o))
	
	mtbcommitsha <- getExportCommit r (exportTreeish o)

	db <- openDb (uuid r)
	writeLockDbWhile db $ do
		changeExport r db tree
		unlessM (Annex.getRead Annex.fast) $ do
			void $ fillExport r db tree mtbcommitsha
	closeDb db

-- | When the treeish is a branch like master or refs/heads/master
-- (but not refs/remotes/...), find the commit it points to
-- and the corresponding remote tracking branch.
--
-- The treeish may also be a subdir within a branch, like master:subdir,
-- that results in this returning the same thing it does for the master
-- branch.
getExportCommit :: Remote -> Git.Ref -> Annex (Maybe (RemoteTrackingBranch, Sha))
getExportCommit r treeish
	| '/' `notElem` fromRef baseref = do
		let tb = mkRemoteTrackingBranch r baseref
		commitsha <- inRepo $ Git.Ref.sha $ Git.Ref.underBase refsheads baseref
		return (fmap (tb, ) commitsha)
	| otherwise = return Nothing
  where
	baseref = Ref $ S8.takeWhile (/= ':') $ fromRef' $ 
		Git.Ref.removeBase refsheads treeish
	refsheads = "refs/heads"

-- | Changes what's exported to the remote. Does not upload any new
-- files, but does delete and rename files already exported to the remote.
changeExport :: Remote -> ExportHandle -> ExportFiltered Git.Ref -> CommandSeek
changeExport r db (ExportFiltered new) = do
	old <- getExport (uuid r)
	recordExportBeginning (uuid r) new
	
	-- Clean up after incomplete export of a tree, in which
	-- the next block of code below may have renamed some files to
	-- temp files. Diff from the incomplete tree to the new tree,
	-- and delete any temp files that the new tree can't use.
	let recover diff = commandAction $
		startRecoverIncomplete r db
			(Git.DiffTree.srcsha diff)
			(Git.DiffTree.file diff)
	forM_ (incompleteExportedTreeishes old) $ \incomplete ->
		mapdiff recover incomplete new
	waitForAllRunningCommandActions

	-- Diff the old and new trees, and delete or rename to new name all
	-- changed files in the export. After this, every file that remains
	-- in the export will have the content from the new treeish.
	-- 
	-- When there was an export conflict, this resolves it.
	--
	-- The ExportTree is also updated here to reflect the new tree.
	case exportedTreeishes old of
		[] -> updateExportTree db emptyTree new
		[oldtreesha] -> do
			diffmap <- mkDiffMap oldtreesha new db
			let seekdiffmap a = mapM_ a (M.toList diffmap)
			-- Rename old files to temp, or delete.
			let deleteoldf = \ek oldf -> commandAction $
				startUnexport' r db oldf ek
			seekdiffmap $ \case
				(ek, (oldf:oldfs, _newf:_)) -> do
					commandAction $
						startMoveToTempName r db oldf ek
					forM_ oldfs (deleteoldf ek)
				(ek, (oldfs, [])) ->
					forM_ oldfs (deleteoldf ek)
				(_ek, ([], _)) -> noop
			waitForAllRunningCommandActions
			-- Rename from temp to new files.
			seekdiffmap $ \case
				(ek, (_oldf:_, newf:_)) -> commandAction $
					startMoveFromTempName r db ek newf
				_ -> noop
			waitForAllRunningCommandActions
		ts -> do
			warning "Resolving export conflict.."
			forM_ ts $ \oldtreesha -> do
				-- Unexport both the srcsha and the dstsha,
				-- because the wrong content may have
				-- been renamed to the dstsha due to the
				-- export conflict.
				let unexportboth d = 
					[ Git.DiffTree.srcsha d 
					, Git.DiffTree.dstsha d
					]
				-- Don't rename to temp, because the
				-- content is unknown; delete instead.
				mapdiff
					(\diff -> commandAction $ startUnexport r db (Git.DiffTree.file diff) (unexportboth diff))
					oldtreesha new
			updateExportTree db emptyTree new
			waitForAllRunningCommandActions
	liftIO $ recordExportTreeCurrent db new

	-- Waiting until now to record the export guarantees that,
	-- if this export is interrupted, there are no files left over
	-- from a previous export, that are not part of this export.
	c <- Annex.getState Annex.errcounter
	when (c == 0) $ do
		recordExportUnderway (uuid r) $ ExportChange
			{ oldTreeish = exportedTreeishes old
			, newTreeish = new
			}
  where
	mapdiff a oldtreesha newtreesha = do
		(diff, cleanup) <- inRepo $
			Git.DiffTree.diffTreeRecursive oldtreesha newtreesha
		sequence_ $ map a diff
		void $ liftIO cleanup

-- Map of old and new filenames for each changed Key in a diff.
type DiffMap = M.Map Key ([TopFilePath], [TopFilePath])

mkDiffMap :: Git.Ref -> Git.Ref -> ExportHandle -> Annex DiffMap
mkDiffMap old new db = do
	(diff, cleanup) <- inRepo $ Git.DiffTree.diffTreeRecursive old new
	diffmap <- M.fromListWith combinedm . concat <$> forM diff mkdm
	void $ liftIO cleanup
	return diffmap
  where
	combinedm (srca, dsta) (srcb, dstb) = (srca ++ srcb, dsta ++ dstb)
	mkdm i = do
		srcek <- getek (Git.DiffTree.srcsha i)
		dstek <- getek (Git.DiffTree.dstsha i)
		updateExportTree' db srcek dstek i
		return $ catMaybes
			[ (, ([Git.DiffTree.file i], [])) <$> srcek
			, (, ([], [Git.DiffTree.file i])) <$> dstek
			]
	getek sha
		| sha `elem` nullShas = return Nothing
		| otherwise = Just <$> exportKey sha

newtype FileUploaded = FileUploaded { fromFileUploaded :: Bool }

newtype AllFilled = AllFilled { fromAllFilled :: Bool }

-- | Upload all exported files that are not yet in the remote.
--
-- Returns True when some files were uploaded (perhaps not all of them).
--
-- Once all exported files have reached the remote, updates the
-- remote tracking branch.
fillExport :: Remote -> ExportHandle -> ExportFiltered Git.Ref -> Maybe (RemoteTrackingBranch, Sha) -> Annex Bool
fillExport r db (ExportFiltered newtree) mtbcommitsha = do
	(l, cleanup) <- inRepo $ Git.LsTree.lsTree
		Git.LsTree.LsTreeRecursive
		(Git.LsTree.LsTreeLong False)
		newtree
	cvar <- liftIO $ newMVar (FileUploaded False)
	allfilledvar <- liftIO $ newMVar (AllFilled True)
	commandActions $
		map (startExport r db cvar allfilledvar) l
	void $ liftIO $ cleanup
	waitForAllRunningCommandActions

	case mtbcommitsha of
		Nothing -> noop
		Just (tb, commitsha) ->
			whenM (liftIO $ fromAllFilled <$> takeMVar allfilledvar) $
				makeRemoteTrackingBranchMergeCommit tb commitsha
					>>= setRemoteTrackingBranch tb
	
	liftIO $ fromFileUploaded <$> takeMVar cvar

startExport :: Remote -> ExportHandle -> MVar FileUploaded -> MVar AllFilled -> Git.LsTree.TreeItem -> CommandStart
startExport r db cvar allfilledvar ti = do
	ek <- exportKey (Git.LsTree.sha ti)
	stopUnless (notrecordedpresent ek) $
		starting ("export " ++ name r) ai si $
			ifM (either (const False) id <$> tryNonAsync (checkPresentExport (exportActions r) ek loc))
				( next $ cleanupExport r db ek loc False
				, do
					liftIO $ modifyMVar_ cvar (pure . const (FileUploaded True))
					performExport r db ek af (Git.LsTree.sha ti) loc allfilledvar
				)
  where
	loc = mkExportLocation f
	f = getTopFilePath (Git.LsTree.file ti)
	af = AssociatedFile (Just f)
	ai = ActionItemTreeFile f
	si = SeekInput []
	notrecordedpresent ek = 
		ifM  (liftIO $ notElem loc <$> getExportedLocation db ek)
			( return True
			-- When content was lost from the remote and
			-- a fsck noticed that, the export db will still
			-- list it as present in the remote. So also check 
			-- location tracking. 
			-- However, git sha keys do not have their locations
			-- tracked, and fsck doesn't check them, so not
			-- for those.
			, if isGitShaKey ek
				then return False
				else notElem (uuid r) <$> loggedLocations ek
			)

performExport :: Remote -> ExportHandle -> Key -> AssociatedFile -> Sha -> ExportLocation -> MVar AllFilled -> CommandPerform
performExport r db ek af contentsha loc allfilledvar = do
	let storer = storeExport (exportActions r)
	sent <- tryNonAsync $ if not (isGitShaKey ek)
		then ifM (inAnnex ek)
			( notifyTransfer Upload af $
				-- alwaysUpload because the same key
				-- could be used for more than one export
				-- location, and concurrently uploading
				-- of the content should still be allowed.
				alwaysUpload (uuid r) ek af Nothing stdRetry $ \pm -> do
					let rollback = void $
						performUnexport r db [ek] loc
					sendAnnex ek rollback $ \f _sz ->
						Remote.action $
							storer f ek loc pm
			, do
				showNote "not available"
				return False
			)
		-- Sending a non-annexed file.
		else withTmpFile "export" $ \tmp h -> do
			b <- catObject contentsha
			liftIO $ L.hPut h b
			liftIO $ hClose h
			Remote.action $
				storer tmp ek loc nullMeterUpdate
	let failedsend = liftIO $ modifyMVar_ allfilledvar (pure . const (AllFilled False))
	case sent of
		Right True -> next $ cleanupExport r db ek loc True
		Right False -> do
			failedsend
			stop
		Left err -> do
			failedsend
			throwM err

cleanupExport :: Remote -> ExportHandle -> Key -> ExportLocation -> Bool -> CommandCleanup
cleanupExport r db ek loc sent = do
	liftIO $ addExportedLocation db ek loc
	when (sent && not (isGitShaKey ek)) $
		logChange ek (uuid r) InfoPresent
	return True

startUnexport :: Remote -> ExportHandle -> TopFilePath -> [Git.Sha] -> CommandStart
startUnexport r db f shas = do
	eks <- forM (filter (`notElem` nullShas) shas) exportKey
	if null eks
		then stop
		else starting ("unexport " ++ name r) ai si $
			performUnexport r db eks loc
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f
	ai = ActionItemTreeFile f'
	si = SeekInput []

startUnexport' :: Remote -> ExportHandle -> TopFilePath -> Key -> CommandStart
startUnexport' r db f ek =
	starting ("unexport " ++ name r) ai si $
		performUnexport r db [ek] loc
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f
	ai = ActionItemTreeFile f'
	si = SeekInput []

-- Unlike a usual drop from a repository, this does not check that
-- numcopies is satisfied before removing the content. Typically an export
-- remote is untrusted, so would not count as a copy anyway.
-- Or, an export may be appendonly, and removing a file from it does
-- not really remove the content, which must be accessible later on.
performUnexport :: Remote -> ExportHandle -> [Key] -> ExportLocation -> CommandPerform
performUnexport r db eks loc = do
	ifM (allM rm eks)
		( next $ cleanupUnexport r db eks loc
		, stop
		)
  where
	rm ek = Remote.action $ removeExport (exportActions r) ek loc

cleanupUnexport :: Remote -> ExportHandle -> [Key] -> ExportLocation -> CommandCleanup
cleanupUnexport r db eks loc = do
	liftIO $ do
		forM_ eks $ \ek ->
			removeExportedLocation db ek loc
		flushDbQueue db

	-- A versionedExport remote supports removeExportLocation to remove
	-- the file from the exported tree, but still retains the content
	-- and allows retrieving it.
	unless (versionedExport (exportActions r)) $ do
		remaininglocs <- liftIO $ 
			concat <$> forM eks (getExportedLocation db)
		when (null remaininglocs) $
			forM_ eks $ \ek ->
				logChange ek (uuid r) InfoMissing
	
	removeEmptyDirectories r db loc eks

startRecoverIncomplete :: Remote -> ExportHandle -> Git.Sha -> TopFilePath -> CommandStart
startRecoverIncomplete r db sha oldf
	| sha `elem` nullShas = stop
	| otherwise = do
		ek <- exportKey sha
		let loc = exportTempName ek
		let ai = ActionItemTreeFile (fromExportLocation loc)
		let si = SeekInput []
		starting ("unexport " ++ name r) ai si $ do
			liftIO $ removeExportedLocation db ek oldloc
			performUnexport r db [ek] loc
  where
	oldloc = mkExportLocation $ getTopFilePath oldf

startMoveToTempName :: Remote -> ExportHandle -> TopFilePath -> Key -> CommandStart
startMoveToTempName r db f ek = case renameExport (exportActions r) of
	Just _ -> starting ("rename " ++ name r) ai si $
		performRename r db ek loc tmploc
	Nothing -> stop
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f
	tmploc = exportTempName ek
	ai = ActionItemOther $ Just $ 
		QuotedPath f' <> " -> " <> QuotedPath (fromExportLocation tmploc)
	si = SeekInput []

startMoveFromTempName :: Remote -> ExportHandle -> Key -> TopFilePath -> CommandStart
startMoveFromTempName r db ek f = case renameExport (exportActions r) of
	Just _ -> stopUnless (liftIO $ elem tmploc <$> getExportedLocation db ek) $
		starting ("rename " ++ name r) ai si $
			performRename r db ek tmploc loc
	Nothing -> stop
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f
	tmploc = exportTempName ek
	ai = ActionItemOther $ Just $
		QuotedPath (fromExportLocation tmploc) <> " -> " <> QuotedPath f'
	si = SeekInput []

performRename :: Remote -> ExportHandle -> Key -> ExportLocation -> ExportLocation -> CommandPerform
performRename r db ek src dest = case renameExport (exportActions r) of
	Just renameaction -> tryNonAsync (renameaction ek src dest) >>= \case
		Right (Just ()) -> next $ cleanupRename r db ek src dest
		Left err -> do
			warning $ UnquotedString $ "rename failed (" ++ show err ++ "); deleting instead"
			fallbackdelete
		Right Nothing -> fallbackdelete
	-- remote does not support renaming
	Nothing -> fallbackdelete
  where
	fallbackdelete = performUnexport r db [ek] src

cleanupRename :: Remote -> ExportHandle -> Key -> ExportLocation -> ExportLocation -> CommandCleanup
cleanupRename r db ek src dest = do
	liftIO $ do
		removeExportedLocation db ek src
		addExportedLocation db ek dest
		flushDbQueue db
	if exportDirectories src /= exportDirectories dest
		then removeEmptyDirectories r db src [ek]
		else return True

-- | Remove empty directories from the export. Call after removing an
-- exported file, and after calling removeExportLocation and flushing the
-- database.
removeEmptyDirectories :: Remote -> ExportHandle -> ExportLocation -> [Key] -> Annex Bool
removeEmptyDirectories r db loc ks
	| null (exportDirectories loc) = return True
	| otherwise = case removeExportDirectory (exportActions r) of
		Nothing -> return True
		Just removeexportdirectory -> do
			ok <- allM (go removeexportdirectory) 
				(reverse (exportDirectories loc))
			unless ok $ liftIO $ do
				-- Add location back to export database, 
				-- so this is tried again next time.
				forM_ ks $ \k ->
					addExportedLocation db k loc
				flushDbQueue db
			return ok
  where
	go removeexportdirectory d = 
		ifM (liftIO $ isExportDirectoryEmpty db d)
			( Remote.action $ removeexportdirectory d
			, return True
			)

-- | A value that has been filtered through the remote's preferred content
-- expression.
newtype ExportFiltered t = ExportFiltered t

-- | Filters the tree to annexed files that are preferred content of the
-- remote, and also including non-annexed files, but not submodules or
-- non-annexed symlinks.
--
-- A log is written with tree items that were filtered out, so they can
-- be added back in when importing from the remote.
filterExport :: Remote -> Git.Ref -> Annex (ExportFiltered Git.Ref)
filterExport r tree = logExportExcluded (uuid r) $ \logwriter -> do
	m <- preferredContentMap
	case M.lookup (uuid r) m of
		Just (matcher, matcherdesc) | not (isEmpty matcher) ->
			ExportFiltered <$> go (Just (matcher, matcherdesc)) logwriter
		_ -> ExportFiltered <$> go Nothing logwriter
  where
	go mmatcher logwriter = do
		g <- Annex.gitRepo
		Git.Tree.adjustTree
			(check mmatcher logwriter)
			[]
			(\_old new -> new)
			[]
			tree
			g

	check mmatcher logwriter ti@(Git.Tree.TreeItem topf mode sha) =
		case toTreeItemType mode of
			-- Don't export submodule entries.
			Just TreeSubmodule -> excluded
			Just TreeSymlink -> checkkey True
			_ -> checkkey False
	  where
		excluded = do
			() <- liftIO $ logwriter ti
			return Nothing

		checkkey issymlink =
			case mmatcher of
				Nothing
					| issymlink -> catKey sha >>= \case
						Just _ -> return (Just ti)
						Nothing -> excluded
					| otherwise -> return (Just ti)
				Just matcher -> catKey sha >>= \case
					Just k -> checkmatcher matcher k
					Nothing
						| issymlink -> excluded
						| otherwise -> return (Just ti)

		checkmatcher matcher k = do
			let mi = MatchingInfo $ ProvidedInfo
				{ providedFilePath = Just $
					-- Match filename relative
					-- to the top of the tree.
					getTopFilePath topf
				, providedKey = Just k
				, providedFileSize = Nothing
				, providedMimeType = Nothing
				, providedMimeEncoding = Nothing
				, providedLinkType = Nothing
				}
			ifM (checkMatcher' matcher mi mempty)
				( return (Just ti)
				, excluded
				)
