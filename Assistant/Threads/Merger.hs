{- git-annex assistant git merge thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Merger where

import Assistant.Common
import Assistant.TransferQueue
import Assistant.BranchChange
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex.Branch
import qualified Git
import qualified Git.Branch
import qualified Command.Sync

thisThread :: ThreadName
thisThread = "Merger"

{- This thread watches for changes to .git/refs/, and handles incoming
 - pushes. -}
mergeThread :: NamedThread
mergeThread = NamedThread "Merger" $ do
	g <- liftAnnex gitRepo
	let dir = Git.localGitDir g </> "refs"
	liftIO $ createDirectoryIfMissing True dir
	let hook a = Just <$> asIO2 (runHandler a)
	addhook <- hook onAdd
	errhook <- hook onErr
	let hooks = mkWatchHooks
		{ addHook = addhook
		, errHook = errhook
		}
	void $ liftIO $ watchDir dir (const False) hooks id
	debug ["watching", dir]

type Handler = FilePath -> Assistant ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: Handler -> FilePath -> Maybe FileStatus -> Assistant ()
runHandler handler file _filestatus =
	either (liftIO . print) (const noop) =<< tryIO <~> handler file

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr msg = error msg

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
onAdd file
	| ".lock" `isSuffixOf` file = noop
	| isAnnexBranch file = do
		branchChanged
		whenM (liftAnnex Annex.Branch.forceUpdate) $
			queueDeferredDownloads Later
	| "/synced/" `isInfixOf` file = do
		mergecurrent =<< liftAnnex (inRepo Git.Branch.current)
	| otherwise = noop
  where
	changedbranch = fileToBranch file
	mergecurrent (Just current)
		| equivBranches changedbranch current = do
			debug
				[ "merging", show changedbranch
				, "into", show current
				]
			void $ liftAnnex  $ Command.Sync.mergeFrom changedbranch
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
