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
import Utility.ThreadLock
import qualified Annex.Queue
import qualified Command.Add
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Git.LsFiles
import qualified Backend
import Annex.Content
import Annex.CatFile
import Git.Types

import Control.Concurrent.STM
import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

#if defined linux_HOST_OS
import Utility.Inotify
import System.INotify
#endif

type Handler = FilePath -> Maybe FileStatus -> DaemonStatusHandle -> Annex (Maybe Change)

watchThread :: ThreadState -> DaemonStatusHandle -> ChangeChan -> IO ()
#if defined linux_HOST_OS
watchThread st dstatus changechan = withINotify $ \i -> do
	runThreadState st $
		showAction "scanning"
	-- This does not return until the startup scan is done.
	-- That can take some time for large trees.
	watchDir i "." (ignored . takeFileName) hooks
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
watchThread = error "so far only available on Linux"
#endif

ignored :: FilePath -> Bool
ignored ".git" = True
ignored ".gitignore" = True
ignored ".gitattributes" = True
ignored _ = False

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

{- Adding a file is tricky; the file has to be replaced with a symlink
 - but this is race prone, as the symlink could be changed immediately
 - after creation. To avoid that race, git add is not used to stage the
 - symlink.
 -
 - Inotify will notice the new symlink, so this Handler does not stage it
 - or return a Change, leaving that to onAddSymlink.
 -
 - During initial directory scan, this will be run for any files that
 - are already checked into git. We don't want to turn those into symlinks,
 - so do a check. This is rather expensive, but only happens during
 - startup.
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
		go = do
			showStart "add" file
			handle =<< Command.Add.ingest file
			noChange
		handle Nothing = showEndFail
		handle (Just key) = do
			Command.Add.link file key True
			showEndOk

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
					| safe (statusChangeTime s) -> noChange
				_ -> addlink link
			where
				safe t = maybe True (> t) (lastRunning daemonstatus)

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
			madeChange file "link"

onDel :: Handler
onDel file _ _dstatus = do
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.unstageFile file)
	madeChange file "rm"

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
	madeChange dir "rmdir"

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr msg _ _dstatus = do
	warning msg
	return Nothing

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)
