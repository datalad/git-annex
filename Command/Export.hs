{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Export where

import Command
import qualified Git
import qualified Git.DiffTree
import qualified Git.LsTree
import qualified Git.Ref
import Git.Types
import Git.FilePath
import Git.Sha
import Types.Key
import Types.Remote
import Annex.Content
import Annex.CatFile
import Logs.Location
import Logs.Export
import Database.Export
import Messages.Progress
import Utility.Tmp

import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = command "export" SectionCommon
	"export content to a remote"
	paramTreeish (seek <$$> optParser)

data ExportOptions = ExportOptions
	{ exportTreeish :: Git.Ref
	, exportRemote :: DeferredParse Remote
	}

optParser :: CmdParamsDesc -> Parser ExportOptions
optParser _ = ExportOptions
	<$> (Git.Ref <$> parsetreeish)
	<*> (parseRemoteOption <$> parseToOption)
  where
	parsetreeish = argument str
		( metavar paramTreeish
		)

-- An export includes both annexed files and files stored in git.
-- For the latter, a SHA1 key is synthesized.
data ExportKey = AnnexKey Key | GitKey Key

asKey :: ExportKey -> Key
asKey (AnnexKey k) = k
asKey (GitKey k) = k

exportKey :: Git.Sha -> Annex ExportKey
exportKey sha = mk <$> catKey sha
  where
	mk (Just k) = AnnexKey k
	mk Nothing = GitKey $ Key
		{ keyName = show sha
		, keyVariety = SHA1Key (HasExt False)
		, keySize = Nothing
		, keyMtime = Nothing
		, keyChunkSize = Nothing
		, keyChunkNum = Nothing
		}

-- To handle renames which swap files, the exported file is first renamed
-- to a stable temporary name based on the key.
exportTempName :: ExportKey -> ExportLocation
exportTempName ek = ExportLocation $ 
	".git-annex-tmp-content-" ++ key2file (asKey (ek))

seek :: ExportOptions -> CommandSeek
seek o = do
	r <- getParsed (exportRemote o)
	unlessM (exportSupported (exportActions r)) $
		giveup "That remote does not support exports."

	new <- fromMaybe (giveup "unknown tree") <$>
		-- Dereference the tree pointed to by the branch, commit,
		-- or tag.
		inRepo (Git.Ref.tree (exportTreeish o))
	old <- getExport (uuid r)
	recordExportBeginning (uuid r) new
	db <- openDb (uuid r)
	
	-- Clean up after incomplete export of a tree, in which
	-- the next block of code below may have renamed some files to
	-- temp files. Diff from the incomplete tree to the new tree,
	-- and delete any temp files that the new tree can't use.
	forM_ (concatMap incompleteExportedTreeish old) $ \incomplete ->
		mapdiff (startUnexportTempName r db . Git.DiffTree.srcsha) incomplete new

	-- Diff the old and new trees, and delete or rename to new name all
	-- changed files in the export. After this, every file that remains
	-- in the export will have the content from the new treeish.
	-- 
	-- (Also, when there was an export conflict, this resolves it.)
	case map exportedTreeish old of
		[] -> return ()
		[oldtreesha] -> do
			-- Rename all old files to temp.
			mapdiff
				(\diff -> startMoveToTempName r db (Git.DiffTree.file diff) (Git.DiffTree.srcsha diff))
				oldtreesha new
			-- Rename from temp to new files.
			mapdiff (\diff -> startMoveFromTempName r db (Git.DiffTree.dstsha diff) (Git.DiffTree.file diff))
				new oldtreesha
			-- Remove all remaining temps.
			mapdiff
				(startUnexportTempName r db . Git.DiffTree.srcsha)
				oldtreesha new
		ts -> do
			warning "Export conflict detected. Different trees have been exported to the same special remote. Resolving.."
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
				-- content is unknown; unexport instead.
				mapdiff
					(\diff -> startUnexport r db (Git.DiffTree.file diff) (unexportboth diff))
					oldtreesha new

	-- Waiting until now to record the export guarantees that,
	-- if this export is interrupted, there are no files left over
	-- from a previous export, that are not part of this export.
	recordExport (uuid r) $ ExportChange
		{ oldTreeish = map exportedTreeish old
		, newTreeish = new
		}

	-- Export everything that is not yet exported.
	(l, cleanup') <- inRepo $ Git.LsTree.lsTree new
	seekActions $ pure $ map (startExport r db) l
	void $ liftIO cleanup'

	closeDb db
  where
	mapdiff a oldtreesha newtreesha = do
		(diff, cleanup) <- inRepo $
			Git.DiffTree.diffTreeRecursive oldtreesha newtreesha
		seekActions $ pure $ map a diff
		void $ liftIO cleanup

startExport :: Remote -> ExportHandle -> Git.LsTree.TreeItem -> CommandStart
startExport r db ti = do
	ek <- exportKey (Git.LsTree.sha ti)
	stopUnless (liftIO $ notElem loc <$> getExportLocation db (asKey ek)) $ do
		showStart "export" f
		next $ performExport r db ek (Git.LsTree.sha ti) loc
  where
	loc = ExportLocation $ toInternalGitPath f
	f = getTopFilePath $ Git.LsTree.file ti

performExport :: Remote -> ExportHandle -> ExportKey -> Sha -> ExportLocation -> CommandPerform
performExport r db ek contentsha loc = do
	let storer = storeExport $ exportActions r
	sent <- case ek of
		AnnexKey k -> ifM (inAnnex k)
			( metered Nothing k $ \m -> do
				let rollback = void $ performUnexport r db [ek] loc
				sendAnnex k rollback
					(\f -> storer f k loc m)
			, do
				showNote "not available"
				return False
			)
		-- Sending a non-annexed file.
		GitKey sha1k -> metered Nothing sha1k $ \m ->
			withTmpFile "export" $ \tmp h -> do
				b <- catObject contentsha
				liftIO $ L.hPut h b
				liftIO $ hClose h
				storer tmp sha1k loc m
	if sent
		then next $ cleanupExport r db ek loc
		else stop

cleanupExport :: Remote -> ExportHandle -> ExportKey -> ExportLocation -> CommandCleanup
cleanupExport r db ek loc = do
	liftIO $ addExportLocation db (asKey ek) loc
	logChange (asKey ek) (uuid r) InfoPresent
	return True

startUnexport :: Remote -> ExportHandle -> TopFilePath -> [Git.Sha] -> CommandStart
startUnexport r db f shas = do
	eks <- forM (filter (/= nullSha) shas) exportKey
	if null eks
		then stop
		else do
			showStart "unexport" f'
			next $ performUnexport r db eks loc
  where
	loc = ExportLocation $ toInternalGitPath f'
	f' = getTopFilePath f

performUnexport :: Remote -> ExportHandle -> [ExportKey] -> ExportLocation -> CommandPerform
performUnexport r db eks loc = do
	ifM (allM (\ek -> removeExport (exportActions r) (asKey ek) loc) eks)
		( next $ cleanupUnexport r db eks loc
		, stop
		)

cleanupUnexport :: Remote -> ExportHandle -> [ExportKey] -> ExportLocation -> CommandCleanup
cleanupUnexport r db eks loc = do
	liftIO $ do
		forM_ eks $ \ek ->
			removeExportLocation db (asKey ek) loc
		-- Flush so that getExportLocation sees this and any
		-- other removals of the key.
		flushDbQueue db
	remaininglocs <- liftIO $ 
		concat <$> forM eks (\ek -> getExportLocation db (asKey ek))
	when (null remaininglocs) $
		forM_ eks $ \ek ->
			logChange (asKey ek) (uuid r) InfoMissing
	return True

startUnexportTempName :: Remote -> ExportHandle -> Git.Sha -> CommandStart
startUnexportTempName r db sha
	| sha == nullSha = stop
	| otherwise = do
		ek <- exportKey sha
		let loc@(ExportLocation f) = exportTempName ek
		stopUnless (liftIO $ elem loc <$> getExportLocation db (asKey ek)) $ do
			showStart "unexport" f
			next $ performUnexport r db [ek] loc

startMoveToTempName :: Remote -> ExportHandle -> TopFilePath -> Git.Sha -> CommandStart
startMoveToTempName r db f sha
	| sha == nullSha = stop
	| otherwise = do
		ek <- exportKey sha
		let tmploc@(ExportLocation tmpf) = exportTempName ek
		showStart "rename" (f' ++ " -> " ++ tmpf)
		next $ performRename r db ek loc tmploc
  where
	loc = ExportLocation $ toInternalGitPath f'
	f' = getTopFilePath f

startMoveFromTempName :: Remote -> ExportHandle -> Git.Sha -> TopFilePath -> CommandStart
startMoveFromTempName r db sha f
	| sha == nullSha = stop
	| otherwise = do
		ek <- exportKey sha
		stopUnless (liftIO $ elem loc <$> getExportLocation db (asKey ek)) $ do
			let tmploc@(ExportLocation tmpf) = exportTempName ek
			showStart "rename" (tmpf ++ " -> " ++ f')
			next $ performRename r db ek tmploc loc
  where
	loc = ExportLocation $ toInternalGitPath f'
	f' = getTopFilePath f

performRename :: Remote -> ExportHandle -> ExportKey -> ExportLocation -> ExportLocation -> CommandPerform
performRename r db ek src dest = do
	ifM (renameExport (exportActions r) (asKey ek) src dest)
		( next $ cleanupRename db ek src dest
		-- In case the special remote does not support renaming,
		-- unexport the src instead.
		, performUnexport r db [ek] src
		)

cleanupRename :: ExportHandle -> ExportKey -> ExportLocation -> ExportLocation -> CommandCleanup
cleanupRename db ek src dest = do
	liftIO $ do
		removeExportLocation db (asKey ek) src
		addExportLocation db (asKey ek) dest
		-- Flush so that getExportLocation sees this.
		flushDbQueue db
	return True
