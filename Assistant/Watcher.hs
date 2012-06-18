{- git-annex assistant tree watcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Watcher where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Committer
import Utility.ThreadScheduler
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Git.LsFiles
import qualified Backend
import qualified Annex
import Annex.Content
import Annex.CatFile
import Git.Types

import Control.Concurrent.STM
import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

#ifdef WITH_INOTIFY
import Utility.Inotify
import System.INotify
#endif
#ifdef WITH_KQUEUE
import Utility.Kqueue
#endif

checkCanWatch :: Annex ()
checkCanWatch = do
#if (WITH_INOTIFY || WITH_KQUEUE)
	unlessM (liftIO (inPath "lsof") <||> Annex.getState Annex.force) $
		needLsof
#else
#if defined linux_HOST_OS
#warning "Building without inotify support; watch mode will be disabled."
#endif
	error "watch mode is not available on this system"
#endif

needLsof :: Annex ()
needLsof = error $ unlines
	[ "The lsof command is needed for watch mode to be safe, and is not in PATH."
	, "To override lsof checks to ensure that files are not open for writing"
	, "when added to the annex, you can use --force"
	, "Be warned: This can corrupt data in the annex, and make fsck complain."
	]

watchThread :: ThreadState -> DaemonStatusHandle -> ChangeChan -> IO ()
#ifdef WITH_INOTIFY
watchThread st dstatus changechan = withINotify $ \i -> do
	runThreadState st $
		showAction "scanning"
	-- This does not return until the startup scan is done.
	-- That can take some time for large trees.
	watchDir i "." ignored hooks
	runThreadState st $
		modifyDaemonStatus dstatus $ \s -> s { scanComplete = True }
	-- Notice any files that were deleted before inotify
	-- was started.
	runThreadState st $ do
		inRepo $ Git.Command.run "add" [Param "--update"]
		showAction "started"
	waitForTermination
	where
		hook a = Just $ runHandler st dstatus changechan a
		hooks = WatchHooks
			{ addHook = hook onAdd
			, delHook = hook onDel
			, addSymlinkHook = hook onAddSymlink
			, delDirHook = hook onDelDir
			, errHook = hook onErr
			}
#else
#ifdef WITH_KQUEUE
watchThread st dstatus changechan = do
	dirs <- scanRecursive "." ignored
	kqueue <- initKqueue dirs
	forever $ do
		changeddir <- waitChange kqueue
		print $ "detected a change in " ++ show changeddir
#else
watchThread = undefined
#endif /* WITH_KQUEUE */
#endif /* WITH_INOTIFY */

ignored :: FilePath -> Bool
ignored = ig . takeFileName
	where
	ig ".git" = True
	ig ".gitignore" = True
	ig ".gitattributes" = True
	ig _ = False

type Handler = FilePath -> Maybe FileStatus -> DaemonStatusHandle -> Annex (Maybe Change)

{- Runs an action handler, inside the Annex monad, and if there was a
 - change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: ThreadState -> DaemonStatusHandle -> ChangeChan -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler st dstatus changechan handler file filestatus = void $ do
	r <- tryIO go
	case r of
		Left e -> print e
		Right Nothing -> noop
		Right (Just change) -> void $
			runChangeChan $ writeTChan changechan change
	where
		go = runThreadState st $ handler file filestatus dstatus

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
 - We could run lsof on the file here to check for other writer.
 - But, that's slow. Instead, a Change is returned that indicates this file
 - still needs to be added. The committer will handle bundles of these
 - Changes at once.
 -}
onAdd :: Handler
onAdd file _filestatus dstatus = do
	ifM (scanComplete <$> getDaemonStatus dstatus)
		( go
		, ifM (null <$> inRepo (Git.LsFiles.notInRepo False [file]))
			( noChange
			, go
			)
		)
	where
		go = madeChange file PendingAddChange

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
onAddSymlink file filestatus dstatus = go =<< Backend.lookupFile file
	where
		go (Just (key, _)) = do
			link <- calcGitLink file key
			ifM ((==) link <$> liftIO (readSymbolicLink file))
				( ensurestaged link =<< getDaemonStatus dstatus
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

onDel :: Handler
onDel file _ _dstatus = do
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
onDelDir dir _ _dstatus = do
	Annex.Queue.addCommand "rm"
		[Params "--quiet -r --cached --ignore-unmatch --"] [dir]
	madeChange dir RmDirChange

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr msg _ _dstatus = do
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
