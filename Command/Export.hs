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

seek :: ExportOptions -> CommandSeek
seek o = do
	r <- getParsed (exportRemote o)
	unlessM (exportSupported (exportActions r)) $
		error "That remote does not support exports."

	new <- fromMaybe (error "unknown tree") <$>
		-- Dereference the tree pointed to by the branch, commit,
		-- or tag.
		inRepo (Git.Ref.tree (exportTreeish o))
	old <- getExport (uuid r)

	when (length old > 1) $
		warning "Export conflict detected. Different trees have been exported to the same special remote. Resolving.."
	
	db <- openDb (uuid r)
	
	-- First, diff the old and new trees and delete all changed
	-- files in the export. Every file that remains in the export will
	-- have the content from the new treeish.
	-- 
	-- (Also, when there was an export conflict, this resolves it.)
	forM_ old $ \oldtreesha -> do
		(diff, cleanup) <- inRepo $
			Git.DiffTree.diffTreeRecursive oldtreesha new
		seekActions $ pure $ map (startUnexport r db) diff
		void $ liftIO cleanup

	-- Waiting until now to record the export guarantees that,
	-- if this export is interrupted, there are no files left over
	-- from a previous export, that are not part of this export.
	recordExport (uuid r) $ ExportChange
		{ oldTreeish = old
		, newTreeish = new
		}

	-- Export everything that is not yet exported.
	(l, cleanup') <- inRepo $ Git.LsTree.lsTree new
	seekActions $ pure $ map (startExport r db) l
	void $ liftIO cleanup'

startExport :: Remote -> ExportHandle -> Git.LsTree.TreeItem -> CommandStart
startExport r db ti = do
	ek <- exportKey (Git.LsTree.sha ti)
	liftIO $ addExportLocation db (asKey ek) loc
	stopUnless (notElem (uuid r) <$> loggedLocations (asKey ek)) $ do
		showStart "export" f
		next $ performExport r ek (Git.LsTree.sha ti) loc
  where
	loc = ExportLocation $ toInternalGitPath f
	f = getTopFilePath $ Git.LsTree.file ti

performExport :: Remote -> ExportKey -> Sha -> ExportLocation -> CommandPerform
performExport r ek contentsha loc = do
	let storer = storeExport $ exportActions r
	sent <- case ek of
		AnnexKey k -> ifM (inAnnex k)
			( metered Nothing k $ \m -> do
				let rollback = void $ performUnexport r ek loc
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
		then next $ cleanupExport r ek
		else stop

cleanupExport :: Remote -> ExportKey -> CommandCleanup
cleanupExport r ek = do
	logChange (asKey ek) (uuid r) InfoPresent
	return True

startUnexport :: Remote -> ExportHandle -> Git.DiffTree.DiffTreeItem -> CommandStart
startUnexport r db diff
	| Git.DiffTree.srcsha diff /= nullSha = do
		showStart "unexport" f
		oldk <- exportKey (Git.DiffTree.srcsha diff)
		liftIO $ removeExportLocation db (asKey oldk) loc
		next $ performUnexport r oldk loc
	| otherwise = stop
  where
	loc = ExportLocation $ toInternalGitPath f
	f = getTopFilePath $ Git.DiffTree.file diff

performUnexport :: Remote -> ExportKey -> ExportLocation -> CommandPerform
performUnexport r ek loc = do
	let remover = removeExport $ exportActions r
	ok <- remover (asKey ek) loc
	if ok
		then next $ cleanupUnexport r ek
		else stop

cleanupUnexport :: Remote -> ExportKey -> CommandCleanup
cleanupUnexport r ek = do
	logChange (asKey ek) (uuid r) InfoMissing
	return True
