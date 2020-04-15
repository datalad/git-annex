{- git-annex assistant git merge thread
 -
 - Copyright 2012-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Threads.Merger where

import Assistant.Common
import Assistant.TransferQueue
import Assistant.BranchChange
import Assistant.Sync
import Utility.DirWatcher
import Utility.DirWatcher.Types
import Annex.CurrentBranch
import qualified Annex
import qualified Annex.Branch
import qualified Git
import qualified Git.Branch
import qualified Git.Ref
import qualified Command.Sync

{- This thread watches for changes to .git/refs/, and handles incoming
 - pushes. -}
mergeThread :: NamedThread
mergeThread = namedThread "Merger" $ do
	g <- liftAnnex gitRepo
	let gitd = fromRawFilePath (Git.localGitDir g)
	let dir = gitd </> "refs"
	liftIO $ createDirectoryUnder gitd dir
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
		diverged <- Annex.Branch.refsWereMerged
			<$> liftAnnex Annex.Branch.forceUpdate
		when diverged $ do
			updateExportTreeFromLogAll
			queueDeferredDownloads "retrying deferred download" Later
	| otherwise = mergecurrent
  where
	changedbranch = fileToBranch file

	mergecurrent =
		mergecurrent' =<< liftAnnex getCurrentBranch
	mergecurrent' currbranch@(Just b, _)
		| changedbranch `isRelatedTo` b =
			whenM (liftAnnex $ inRepo $ Git.Branch.changed b changedbranch) $ do
				debug
					[ "merging", Git.fromRef changedbranch
					, "into", Git.fromRef b
					]
				void $ liftAnnex $ do
					cmode <- annexCommitMode <$> Annex.getGitConfig
					Command.Sync.merge
						currbranch Command.Sync.mergeConfig
						def
						cmode
						changedbranch
	mergecurrent' _ = noop

{- Is the first branch a synced branch or remote tracking branch related
 - to the second branch, which should be merged into it? -}
isRelatedTo :: Git.Ref -> Git.Ref -> Bool
isRelatedTo x y
	| basex /= takeDirectory basex ++ "/" ++ basey = False
	| "/synced/" `isInfixOf` Git.fromRef x = True
	| "refs/remotes/" `isPrefixOf` Git.fromRef x = True
	| otherwise = False
  where
	basex = Git.fromRef $ Git.Ref.base x
	basey = Git.fromRef $ Git.Ref.base y

isAnnexBranch :: FilePath -> Bool
isAnnexBranch f = n `isSuffixOf` f
  where
	n = '/' : Git.fromRef Annex.Branch.name

fileToBranch :: FilePath -> Git.Ref
fileToBranch f = Git.Ref $ encodeBS' $ "refs" </> base
  where
	base = Prelude.last $ split "/refs/" f
