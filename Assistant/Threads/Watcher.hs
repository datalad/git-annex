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
import qualified Annex
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Backend
import Annex.Content
import Annex.CatFile
import Git.Types

import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

checkCanWatch :: Annex ()
checkCanWatch
	| canWatch = 
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
	startup <- asIO startupScan
	addhook <- hook onAdd
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
		liftAnnex $ do
			inRepo $ Git.Command.run "add" [Param "--update"]
			showAction "started"
		
		dstatus <- getAssistant daemonStatusHandle
		liftIO $ modifyDaemonStatus_ dstatus $
			\s -> s { scanComplete = True }

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

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
onAddSymlink file filestatus = go =<< liftAnnex (Backend.lookupFile file)
  where
	go (Just (key, _)) = do
		link <- liftAnnex $ calcGitLink file key
		ifM ((==) link <$> liftIO (readSymbolicLink file))
			( do
				s <- daemonStatus
				checkcontent key s
				ensurestaged link s
			, do
				liftIO $ removeFile file
				liftIO $ createSymbolicLink link file
				checkcontent key =<< daemonStatus
				addlink link
			)
	go Nothing = do -- other symlink
		link <- liftIO (readSymbolicLink file)
		ensurestaged link =<< daemonStatus

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
	ensurestaged link daemonstatus
		| scanComplete daemonstatus = addlink link
		| otherwise = case filestatus of
			Just s
				| not (afterLastDaemonRun (statusChangeTime s) daemonstatus) -> noChange
			_ -> addlink link

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
	 - scan, handle getting or dropping the key's content. -}
	checkcontent key daemonstatus
		| scanComplete daemonstatus = do
			present <- liftAnnex $ inAnnex key
			dstatus <- getAssistant daemonStatusHandle
			unless present $ do
				transferqueue <- getAssistant transferQueue
				liftAnnex $ queueTransfers Next transferqueue
					dstatus key (Just file) Download
			liftAnnex $ handleDrops dstatus present key (Just file)
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
	dstatus <- getAssistant daemonStatusHandle
	void $ liftIO $ addAlert dstatus $ warningAlert "watcher" msg
	noChange

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. This avoids a race if git add is used, where the symlink is
 - changed to something else immediately after creation.
 -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)
