{- git-annex assistant thread to handle reported problems with repositories
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.ProblemChecker (
	problemCheckerThread
) where

import Assistant.Common
import Utility.ThreadScheduler
import Assistant.Types.UrlRenderer
import Assistant.Alert
import Remote
import qualified Types.Remote as Remote
import qualified Git.Fsck
import Assistant.Repair
import qualified Git
import Assistant.RepoProblem
import Assistant.Sync
import Annex.UUID

{- Waits for problems with a repo, and tries to fsck the repo and repair
 - the problem. -}
problemCheckerThread :: UrlRenderer -> NamedThread
problemCheckerThread urlrenderer = namedThread "ProblemChecker" $ forever $ do
	mapM_ (handleProblem urlrenderer)
		=<< nub <$> getRepoProblems
	liftIO $ threadDelaySeconds (Seconds 60)

handleProblem :: UrlRenderer -> UUID -> Assistant ()
handleProblem urlrenderer u = ifM ((==) u <$> liftAnnex getUUID)
	( handleLocalRepoProblem urlrenderer
	, maybe noop (handleRemoteProblem urlrenderer)
		=<< liftAnnex (remoteFromUUID u)
	)

handleRemoteProblem :: UrlRenderer -> Remote -> Assistant ()
handleRemoteProblem urlrenderer rmt
	| Git.repoIsLocal r && not (Git.repoIsLocalUnknown r) =
		whenM (liftIO $ checkAvailable True rmt) $ do
			repairStaleGitLocks r
			fsckresults <- showFscking urlrenderer (Just $ Remote.name rmt) $ tryNonAsync $
				Git.Fsck.findBroken True r
			whenM (repairWhenNecessary urlrenderer (Remote.uuid rmt) (Just rmt) fsckresults) $
				syncRemote rmt
	| otherwise = noop
  where
	r = Remote.repo rmt

handleLocalRepoProblem :: UrlRenderer -> Assistant ()
handleLocalRepoProblem urlrenderer = do
	repairStaleGitLocks =<< liftAnnex gitRepo
