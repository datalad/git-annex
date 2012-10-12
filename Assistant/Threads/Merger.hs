{- git-annex assistant git merge thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Merger where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex.Branch
import qualified Git
import qualified Git.Merge
import qualified Git.Branch

thisThread :: ThreadName
thisThread = "Merger"

{- This thread watches for changes to .git/refs/, and handles incoming
 - pushes. -}
mergeThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> NamedThread
mergeThread st dstatus transferqueue = thread $ do
	g <- runThreadState st gitRepo
	let dir = Git.localGitDir g </> "refs"
	createDirectoryIfMissing True dir
	let hook a = Just $ runHandler st dstatus transferqueue a
	let hooks = mkWatchHooks
		{ addHook = hook onAdd
		, errHook = hook onErr
		}
	void $ watchDir dir (const False) hooks id
	debug thisThread ["watching", dir]
	where
		thread = NamedThread thisThread

type Handler = ThreadState -> DaemonStatusHandle -> TransferQueue -> FilePath -> Maybe FileStatus -> IO ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler st dstatus transferqueue handler file filestatus = void $
        either print (const noop) =<< tryIO go
        where
                go = handler st dstatus transferqueue file filestatus

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr _ _ _ msg _ = error msg

{- Called when a new branch ref is written.
 -
 - This relies on git's atomic method of updating branch ref files,
 - which is to first write the new file to .lock, and then rename it
 - over the old file. So, ignore .lock files, and the rename ensures
 - the watcher sees a new file being added on each update.
 -
 - At startup, synthetic add events fire, causing this to run, but that's
 - ok; it ensures that any changes pushed since the last time the assistant
 - ran are merged in.
 -}
onAdd :: Handler
onAdd st dstatus transferqueue file _
	| ".lock" `isSuffixOf` file = noop
	| isAnnexBranch file = runThreadState st $
		whenM Annex.Branch.forceUpdate $
			queueDeferredDownloads Later transferqueue dstatus
	| "/synced/" `isInfixOf` file = runThreadState st $ do
		mergecurrent =<< inRepo Git.Branch.current
	| otherwise = noop
	where
		changedbranch = fileToBranch file
		mergecurrent (Just current)
			| equivBranches changedbranch current = do
				liftIO $ debug thisThread
					[ "merging"
					, show changedbranch
					, "into"
					, show current
					]
				void $ inRepo $
					Git.Merge.mergeNonInteractive changedbranch
		mergecurrent _ = noop

equivBranches :: Git.Ref -> Git.Ref -> Bool
equivBranches x y = base x == base y
	where
		base = takeFileName . show

isAnnexBranch :: FilePath -> Bool
isAnnexBranch f = n `isSuffixOf` f
	where
		n = "/" ++ show Annex.Branch.name

fileToBranch :: FilePath -> Git.Ref
fileToBranch f = Git.Ref $ "refs" </> base
	where
		base = Prelude.last $ split "/refs/" f
