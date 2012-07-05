{- git-annex assistant tree watcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.Watcher where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.TransferQueue
import Logs.Transfer
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Git.LsFiles
import qualified Backend
import qualified Command.Add
import Annex.Content
import Annex.CatFile
import Git.Types

import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

checkCanWatch :: Annex ()
checkCanWatch
	| canWatch = 
		unlessM (liftIO (inPath "lsof") <||> Annex.getState Annex.force) $
			needLsof
	| otherwise = error "watch mode is not available on this system"

needLsof :: Annex ()
needLsof = error $ unlines
	[ "The lsof command is needed for watch mode to be safe, and is not in PATH."
	, "To override lsof checks to ensure that files are not open for writing"
	, "when added to the annex, you can use --force"
	, "Be warned: This can corrupt data in the annex, and make fsck complain."
	]

watchThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> IO ()
watchThread st dstatus transferqueue changechan = void $ watchDir "." ignored hooks startup
	where
		startup = statupScan st dstatus
		hook a = Just $ runHandler st dstatus transferqueue changechan a
		hooks = WatchHooks
			{ addHook = hook onAdd
			, delHook = hook onDel
			, addSymlinkHook = hook onAddSymlink
			, delDirHook = hook onDelDir
			, errHook = hook onErr
			}

{- Initial scartup scan. The action should return once the scan is complete. -}
statupScan :: ThreadState -> DaemonStatusHandle -> IO a -> IO a
statupScan st dstatus scanner = do
	runThreadState st $
		showAction "scanning"
	r <- scanner
	runThreadState st $
		modifyDaemonStatus dstatus $ \s -> s { scanComplete = True }

	-- Notice any files that were deleted before watching was started.
	runThreadState st $ do
		inRepo $ Git.Command.run "add" [Param "--update"]
		showAction "started"

	return r

ignored :: FilePath -> Bool
ignored = ig . takeFileName
	where
	ig ".git" = True
	ig ".gitignore" = True
	ig ".gitattributes" = True
	ig _ = False

type Handler = FilePath -> Maybe FileStatus -> DaemonStatusHandle -> TransferQueue -> Annex (Maybe Change)

{- Runs an action handler, inside the Annex monad, and if there was a
 - change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler st dstatus transferqueue changechan handler file filestatus = void $ do
	r <- tryIO go
	case r of
		Left e -> print e
		Right Nothing -> noop
		Right (Just change) -> recordChange changechan change
	where
		go = runThreadState st $ handler file filestatus dstatus transferqueue

{- During initial directory scan, this will be run for any regular files
 - that are already checked into git. We don't want to turn those into
 - symlinks, so do a check. This is rather expensive, but only happens
 - during startup.
 -
 - It's possible for the file to still be open for write by some process.
 - This can happen in a few ways; one is if two processes had the file open
 - and only one has just closed it. We want to avoid adding a file to the
 - annex that is open for write, to avoid anything being able to change it.
 -
 - We could run lsof on the file here to check for other writers.
 - But, that's slow, and even if there is currently a writer, we will want
 - to add the file *eventually*. Instead, the file is locked down as a hard
 - link in a temp directory, with its write bits disabled, for later
 - checking with lsof, and a Change is returned containing a KeySource
 - using that hard link. The committer handles running lsof and finishing
 - the add.
 -}
onAdd :: Handler
onAdd file filestatus dstatus _
	| maybe False isRegularFile filestatus = do
		ifM (scanComplete <$> getDaemonStatus dstatus)
			( go
			, ifM (null <$> inRepo (Git.LsFiles.notInRepo False [file]))
				( noChange
				, go
				)
			)
	| otherwise = noChange
	where
		go = pendingAddChange =<< Command.Add.lockDown file

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
onAddSymlink file filestatus dstatus transferqueue = go =<< Backend.lookupFile file
	where
		go (Just (key, _)) = do
			link <- calcGitLink file key
			ifM ((==) link <$> liftIO (readSymbolicLink file))
				( do
					s <- getDaemonStatus dstatus
					checkcontent key s
					ensurestaged link s
				, do
					liftIO $ removeFile file
					liftIO $ createSymbolicLink link file
					addlink link
				)
		go Nothing = do -- other symlink
			link <- liftIO (readSymbolicLink file)
			ensurestaged link =<< getDaemonStatus dstatus

		{- This is often called on symlinks that are already
		 - staged correctly. A symlink may have been deleted
		 - and being re-added, or added when the watcher was
		 - not running. So they're normally restaged to make sure.
		 -
		 - As an optimisation, during the status scan, avoid
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

		{- For speed, tries to reuse the existing blob for
		 - the symlink target. -}
		addlink link = do
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

		{- When a new link appears, after the startup scan,
		 - try to get the key's content. -}
		checkcontent key daemonstatus
			| scanComplete daemonstatus = unlessM (inAnnex key) $
				queueTransfers transferqueue dstatus
					key (Just file) Download
			| otherwise = noop

onDel :: Handler
onDel file _ _dstatus _ = do
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
onDelDir dir _ _dstatus _ = do
	Annex.Queue.addCommand "rm"
		[Params "--quiet -r --cached --ignore-unmatch --"] [dir]
	madeChange dir RmDirChange

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr msg _ _dstatus _ = do
	warning msg
	return Nothing

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. This avoids a race if git add is used, where the symlink is
 - changed to something else immediately after creation.
 -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)
