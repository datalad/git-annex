{- git-annex assistant git merge thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Merger where

import Assistant.Common
import Assistant.ThreadedMonad
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex.Branch
import qualified Git
import qualified Git.Merge
import qualified Git.Branch
import qualified Git.Command as Git

thisThread :: ThreadName
thisThread = "Merger"

{- This thread watches for changes to .git/refs/heads/synced/,
 - which indicate incoming pushes. It merges those pushes into the
 - currently checked out branch. -}
mergeThread :: ThreadState -> NamedThread
mergeThread st = thread $ do
	g <- runThreadState st $ fromRepo id
	let dir = Git.localGitDir g </> "refs" </> "heads" </> "synced"
	createDirectoryIfMissing True dir
	let hook a = Just $ runHandler g a
	let hooks = mkWatchHooks
		{ addHook = hook onAdd
		, errHook = hook onErr
		}
	void $ watchDir dir (const False) hooks id
	debug thisThread ["watching", dir]
	where
		thread = NamedThread thisThread

type Handler = Git.Repo -> FilePath -> Maybe FileStatus -> IO ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: Git.Repo -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler g handler file filestatus = void $
        either print (const noop) =<< tryIO go
        where
                go = handler g file filestatus

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr _ msg _ = error msg

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
onAdd g file _
	| ".lock" `isSuffixOf` file = noop
	| isAnnexBranch file = noop
	| "/synced/" `isInfixOf` file = go =<< Git.Branch.current g
	| otherwise = noop
	where
		changedbranch = fileToBranch file
		go (Just current)
			| equivBranches changedbranch current = do
				liftIO $ debug thisThread
					[ "merging"
					, show changedbranch
					, "into"
					, show current
					]
				void $ Git.Merge.mergeNonInteractive changedbranch g
				when ("fallback/" `isInfixOf` (show changedbranch)) $
					void $ Git.runBool "branch"
						[ Param "-D"
						, Param $ show changedbranch
						] g
		go _ = noop

equivBranches :: Git.Ref -> Git.Ref -> Bool
equivBranches x y = base x == base y
	where
		base = takeFileName . show

isAnnexBranch :: FilePath -> Bool
isAnnexBranch f = n `isSuffixOf` f
	where
		n = "/" ++ show Annex.Branch.name

fileToBranch :: FilePath -> Git.Ref
fileToBranch f = Git.Ref $ "refs" </> "heads" </> base
	where
		base = Prelude.last $ split "/refs/heads/" f
