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
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex.Branch
import qualified Git
import qualified Git.Branch
import qualified Command.Sync
import Annex.TaggedPush
import Remote (remoteFromUUID)

import qualified Data.Set as S

{- This thread watches for changes to .git/refs/, and handles incoming
 - pushes. -}
mergeThread :: NamedThread
mergeThread = namedThread "Merger" $ do
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
		diverged <- liftAnnex Annex.Branch.forceUpdate
		when diverged $
			unlessM handleDesynced $
				queueDeferredDownloads "retrying deferred download" Later
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

	handleDesynced = case branchTaggedBy changedbranch of
		Nothing -> return False
		Just u -> do
			s <- desynced <$> getDaemonStatus				
			if S.member u s
				then do
					modifyDaemonStatus_ $ \st -> st
						{ desynced = S.delete u s }
					mr <- liftAnnex $ remoteFromUUID u
					case mr of
						Just r -> do
							addScanRemotes True [r]
							return True
						Nothing -> return False
				else return False

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
