{- git-annex assistant tree watcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Watcher (
	watchThread,
	checkCanWatch,
	needLsof,
	stageSymlink,
	onAddSymlink,
	runHandler,
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.Types.Changes
import Assistant.TransferQueue
import Assistant.Alert
import Assistant.Drop
import Logs.Transfer
import Utility.DirWatcher
import Utility.Types.DirWatcher
import Utility.Lsof
import qualified Annex
import qualified Annex.Queue
import qualified Git
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Git.LsFiles as LsFiles
import qualified Backend
import Annex.Content
import Annex.Direct
import Annex.Content.Direct
import Annex.CatFile
import Git.Types
import Config

import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

checkCanWatch :: Annex ()
checkCanWatch
	| canWatch = do
		liftIO setupLsof
		unlessM (liftIO (inPath "lsof") <||> Annex.getState Annex.force)
			needLsof
	| otherwise = error "watch mode is not available on this system"

needLsof :: Annex ()
needLsof = error $ unlines
	[ "The lsof command is needed for watch mode to be safe, and is not in PATH."
	, "To override lsof checks to ensure that files are not open for writing"
	, "when added to the annex, you can use --force"
	, "Be warned: This can corrupt data in the annex, and make fsck complain."
	]

watchThread :: NamedThread
watchThread = NamedThread "Watcher" $ do
	startup <- asIO1 startupScan
	direct <- liftAnnex isDirect
	addhook <- hook $ if direct then onAddDirect else onAdd
	delhook <- hook onDel
	addsymlinkhook <- hook onAddSymlink
	deldirhook <- hook onDelDir
	errhook <- hook onErr
	let hooks = mkWatchHooks
		{ addHook = addhook
		, delHook = delhook
		, addSymlinkHook = addsymlinkhook
		, delDirHook = deldirhook
		, errHook = errhook
		}
	void $ liftIO $ watchDir "." ignored hooks startup
	debug [ "watching", "."]
  where
	hook a = Just <$> asIO2 (runHandler a)

{- Initial scartup scan. The action should return once the scan is complete. -}
startupScan :: IO a -> Assistant a
startupScan scanner = do
	liftAnnex $ showAction "scanning"
	alertWhile' startupScanAlert $ do
		r <- liftIO $ scanner

		-- Notice any files that were deleted before
		-- watching was started.
		top <- liftAnnex $ fromRepo Git.repoPath
		(fs, cleanup) <- liftAnnex $ inRepo $ LsFiles.deleted [top]
		forM_ fs $ \f -> do
			liftAnnex $ Annex.Queue.addUpdateIndex =<<
				inRepo (Git.UpdateIndex.unstageFile f)
			maybe noop recordChange =<< madeChange f RmChange
		void $ liftIO $ cleanup
		
		liftAnnex $ showAction "started"
		
		modifyDaemonStatus_ $ \s -> s { scanComplete = True }

		return (True, r)

ignored :: FilePath -> Bool
ignored = ig . takeFileName
  where
	ig ".git" = True
	ig ".gitignore" = True
	ig ".gitattributes" = True
	ig _ = False

type Handler = FilePath -> Maybe FileStatus -> Assistant (Maybe Change)

{- Runs an action handler, and if there was a change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: Handler -> FilePath -> Maybe FileStatus -> Assistant ()
runHandler handler file filestatus = void $ do
	r <- tryIO <~> handler file filestatus
	case r of
		Left e -> liftIO $ print e
		Right Nothing -> noop
		Right (Just change) -> do
			-- Just in case the commit thread is not
			-- flushing the queue fast enough.
			liftAnnex $ Annex.Queue.flushWhenFull
			recordChange change

onAdd :: Handler
onAdd file filestatus
	| maybe False isRegularFile filestatus = pendingAddChange file
	| otherwise = noChange

{- In direct mode, add events are received for both new files, and
 - modified existing files. Or, in some cases, existing files that have not
 - really been modified. -}
onAddDirect :: Handler
onAddDirect file fs = do
	v <- liftAnnex $ catKeyFile file
	case (v, fs) of
		(Just key, Just filestatus) ->
			ifM (liftAnnex $ changedFileStatus key filestatus)
				( do
					liftAnnex $ changedDirect key file
					pendingAddChange file
				, noChange
				)
		_ -> pendingAddChange file

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
onAddSymlink file filestatus = go =<< liftAnnex (Backend.lookupFile file)
  where
	go (Just (key, _)) = do
		link <- liftAnnex $ calcGitLink file key
		ifM ((==) (Just link) <$> liftIO (catchMaybeIO $ readSymbolicLink file))
			( do
				s <- getDaemonStatus
				checkcontent key s
				ensurestaged (Just link) s
			, do
				liftIO $ removeFile file
				liftIO $ createSymbolicLink link file
				checkcontent key =<< getDaemonStatus
				addlink link
			)
	go Nothing = do -- other symlink
		mlink <- liftIO (catchMaybeIO $ readSymbolicLink file)
		ensurestaged mlink =<< getDaemonStatus

	{- This is often called on symlinks that are already
	 - staged correctly. A symlink may have been deleted
	 - and being re-added, or added when the watcher was
	 - not running. So they're normally restaged to make sure.
	 -
	 - As an optimisation, during the startup scan, avoid
	 - restaging everything. Only links that were created since
	 - the last time the daemon was running are staged.
	 - (If the daemon has never ran before, avoid staging
	 - links too.)
	 -}
	ensurestaged (Just link) daemonstatus
		| scanComplete daemonstatus = addlink link
		| otherwise = case filestatus of
			Just s
				| not (afterLastDaemonRun (statusChangeTime s) daemonstatus) -> noChange
			_ -> addlink link
	ensurestaged Nothing _ = noChange

	{- For speed, tries to reuse the existing blob for symlink target. -}
	addlink link = do
		debug ["add symlink", file]
		liftAnnex $ do
			v <- catObjectDetails $ Ref $ ':':file
			case v of
				Just (currlink, sha)
					| s2w8 link == L.unpack currlink ->
						stageSymlink file sha
				_ -> do
					sha <- inRepo $
						Git.HashObject.hashObject BlobObject link
					stageSymlink file sha
		madeChange file LinkChange

	{- When a new link appears, or a link is changed, after the startup
	 - scan, handle getting or dropping the key's content.
	 - Also, moving or copying a link may caused it be be transferred
	 - elsewhere, so check that too. -}
	checkcontent key daemonstatus
		| scanComplete daemonstatus = do
			present <- liftAnnex $ inAnnex key
			if present
				then queueTransfers Next key (Just file) Upload
				else queueTransfers Next key (Just file) Download
			handleDrops present key (Just file) Nothing
		| otherwise = noop

onDel :: Handler
onDel file _ = do
	debug ["file deleted", file]
	liftAnnex $ 
		Annex.Queue.addUpdateIndex =<<
			inRepo (Git.UpdateIndex.unstageFile file)
	madeChange file RmChange

{- A directory has been deleted, or moved, so tell git to remove anything
 - that was inside it from its cache. Since it could reappear at any time,
 - use --cached to only delete it from the index. 
 -
 - Note: This could use unstageFile, but would need to run another git
 - command to get the recursive list of files in the directory, so rm is
 - just as good. -}
onDelDir :: Handler
onDelDir dir _ = do
	debug ["directory deleted", dir]
	liftAnnex $ Annex.Queue.addCommand "rm"
		[Params "--quiet -r --cached --ignore-unmatch --"] [dir]
	madeChange dir RmDirChange

{- Called when there's an error with inotify or kqueue. -}
onErr :: Handler
onErr msg _ = do
	liftAnnex $ warning msg
	void $ addAlert $ warningAlert "watcher" msg
	noChange

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. This avoids a race if git add is used, where the symlink is
 - changed to something else immediately after creation. It also allows
 - direct mode to work.
 -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)
