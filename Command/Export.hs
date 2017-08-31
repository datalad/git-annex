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
import Git.Types
import Git.Sha
import Git.FilePath
import Types.Key
import Types.Remote
import Annex.Content
import Annex.CatFile
import Logs.Location
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
	let oldtreeish = emptyTree -- XXX temporary

	-- First, diff the old and new trees and update all changed
	-- files in the export.
	(diff, cleanup) <- inRepo $
		Git.DiffTree.diffTreeRecursive oldtreeish (exportTreeish o)
	seekActions $ pure $ map (startDiff r) diff
	void $ liftIO cleanup

	-- In case a previous export was incomplete, make a pass
	-- over the whole tree and export anything that is not
	-- yet exported.
	(l, cleanup') <- inRepo $ Git.LsTree.lsTree (exportTreeish o)
	seekActions $ pure $ map (start r) l
	void $ liftIO cleanup'

startDiff :: Remote -> Git.DiffTree.DiffTreeItem -> CommandStart
startDiff r diff
	| Git.DiffTree.dstsha diff == nullSha = do
		showStart "unexport" f
		oldk <- exportKey (Git.DiffTree.srcsha diff)
		next $ performUnexport r oldk loc
	| otherwise = do
		showStart "export" f
		k <- exportKey (Git.DiffTree.dstsha diff)
		next $ performExport r k (Git.DiffTree.dstsha diff) loc
  where
	loc = ExportLocation $ toInternalGitPath $ 
		getTopFilePath $ Git.DiffTree.file diff
	f = getTopFilePath $ Git.DiffTree.file diff

start :: Remote -> Git.LsTree.TreeItem -> CommandStart
start r ti = do
	ek <- exportKey (Git.LsTree.sha ti)
	stopUnless (elem (uuid r) <$> loggedLocations (asKey ek)) $
		next $ performExport r ek (Git.LsTree.sha ti) loc
  where
	loc = ExportLocation $ toInternalGitPath $
		getTopFilePath $ Git.LsTree.file ti

performExport :: Remote -> ExportKey -> Sha -> ExportLocation -> CommandPerform
performExport r ek contentsha loc = case storeExport r of
	Nothing -> error "remote does not support exporting files"
	Just storer -> do
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

performUnexport :: Remote -> ExportKey -> ExportLocation -> CommandPerform
performUnexport r ek loc = case removeExport r of
	Nothing -> error "remote does not support removing exported files"
	Just remover -> do
		ok <- remover (asKey ek) loc
		if ok
			then next $ cleanupUnexport r ek
			else stop

cleanupUnexport :: Remote -> ExportKey -> CommandCleanup
cleanupUnexport r ek = do
	logChange (asKey ek) (uuid r) InfoMissing
	return True
