{- git-annex assistant git merge thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
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
import Utility.DirWatcher.Types
import qualified Annex.Branch
import qualified Git
import qualified Git.Branch
import qualified Command.Sync
import Annex.TaggedPush
import Remote (remoteFromUUID)

import qualified Data.Set as S
import qualified Data.Text as T

{- This thread watches for changes to .git/refs/, and handles incoming
 - pushes. -}
mergeThread :: NamedThread
mergeThread = namedThread "Merger" $ do
	g <- liftAnnex gitRepo
	let dir = Git.localGitDir g </> "refs"
	liftIO $ createDirectoryIfMissing True dir
	let hook a = Just <$> asIO2 (runHandler a)
	changehook <- hook onChange
	errhook <- hook onErr
	let hooks = mkWatchHooks
		{ addHook = changehook
		, modifyHook = changehook
		, errHook = errhook
		}
	void $ liftIO $ watchDir dir (const False) True hooks id
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
onErr = error

{- Called when a new branch ref is written, or a branch ref is modified.
 -
 - At startup, synthetic add events fire, causing this to run, but that's
 - ok; it ensures that any changes pushed since the last time the assistant
 - ran are merged in.
 -}
onChange :: Handler
onChange file
	| ".lock" `isSuffixOf` file = noop
	| isAnnexBranch file = do
		branchChanged
		diverged <- liftAnnex Annex.Branch.forceUpdate
		when diverged $
			unlessM handleDesynced $
				queueDeferredDownloads "retrying deferred download" Later
	| "/synced/" `isInfixOf` file =
		mergecurrent =<< liftAnnex (join Command.Sync.getCurrBranch)
	| otherwise = noop
  where
	changedbranch = fileToBranch file

	mergecurrent currbranch@(Just b, _)
		| equivBranches changedbranch b =
			whenM (liftAnnex $ inRepo $ Git.Branch.changed b changedbranch) $ do
				debug
					[ "merging", Git.fromRef changedbranch
					, "into", Git.fromRef b
					]
				void $ liftAnnex $ Command.Sync.merge
					currbranch Command.Sync.mergeConfig
					Git.Branch.AutomaticCommit
					changedbranch
	mergecurrent _ = noop

	handleDesynced = case fromTaggedBranch changedbranch of
		Nothing -> return False
		Just (u, info) -> do
			mr <- liftAnnex $ remoteFromUUID u
			case mr of
				Nothing -> return False
				Just r -> do
					s <- desynced <$> getDaemonStatus
					if S.member u s || Just (T.unpack $ getXMPPClientID r) == info
						then do
							modifyDaemonStatus_ $ \st -> st
								{ desynced = S.delete u s }
							addScanRemotes True [r]
							return True
						else return False

equivBranches :: Git.Ref -> Git.Ref -> Bool
equivBranches x y = base x == base y
  where
	base = takeFileName . Git.fromRef

isAnnexBranch :: FilePath -> Bool
isAnnexBranch f = n `isSuffixOf` f
  where
	n = '/' : Git.fromRef Annex.Branch.name

fileToBranch :: FilePath -> Git.Ref
fileToBranch f = Git.Ref $ "refs" </> base
  where
	base = Prelude.last $ split "/refs/" f
