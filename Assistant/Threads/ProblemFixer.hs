{- git-annex assistant thread to handle fixing problems with repositories
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Threads.ProblemFixer (
	problemFixerThread
) where

import Assistant.Common
import Assistant.Types.RepoProblem
import Assistant.RepoProblem
import Assistant.Types.UrlRenderer
import Assistant.Alert
import Remote
import qualified Types.Remote as Remote
import qualified Git.Fsck
import Assistant.Repair
import qualified Git
import Annex.UUID
import Utility.ThreadScheduler

{- Waits for problems with a repo, and tries to fsck the repo and repair
 - the problem. -}
problemFixerThread :: UrlRenderer -> NamedThread
problemFixerThread urlrenderer = namedThread "ProblemFixer" $
	go =<< getRepoProblems
  where
	go problems = do
		mapM_ (handleProblem urlrenderer) problems
		liftIO $ threadDelaySeconds (Seconds 60)
		-- Problems may have been re-reported while they were being
		-- fixed, so ignore those. If a new unique problem happened
		-- 60 seconds after the last was fixed, we're unlikely
		-- to do much good anyway.
		go =<< filter (\p -> not (any (sameRepoProblem p) problems))
			<$> getRepoProblems

handleProblem :: UrlRenderer -> RepoProblem -> Assistant ()
handleProblem urlrenderer repoproblem = do
	fixed <- ifM ((==) (problemUUID repoproblem) <$> liftAnnex getUUID)
		( handleLocalRepoProblem urlrenderer
		, maybe (return False) (handleRemoteProblem urlrenderer)
			=<< liftAnnex (remoteFromUUID $ problemUUID repoproblem)
		)
	when fixed $
		liftIO $ afterFix repoproblem

handleRemoteProblem :: UrlRenderer -> Remote -> Assistant Bool
handleRemoteProblem urlrenderer rmt = do
	repo <- liftAnnex $ Remote.getRepo rmt
	handleRemoteProblem' repo urlrenderer rmt

handleRemoteProblem' :: Git.Repo -> UrlRenderer -> Remote -> Assistant Bool
handleRemoteProblem' repo urlrenderer rmt
	| Git.repoIsLocal repo && not (Git.repoIsLocalUnknown repo) =
		ifM (liftAnnex $ checkAvailable True rmt)
			( do
				fixedlocks <- repairStaleGitLocks repo
				fsckresults <- showFscking urlrenderer (Just rmt) $ tryNonAsync $
					Git.Fsck.findBroken True True repo
				repaired <- repairWhenNecessary urlrenderer (Remote.uuid rmt) (Just rmt) fsckresults
				return $ fixedlocks || repaired
			, return False
			)
	| otherwise = return False

{- This is not yet used, and should probably do a fsck. -}
handleLocalRepoProblem :: UrlRenderer -> Assistant Bool
handleLocalRepoProblem _urlrenderer = do
	repairStaleGitLocks =<< liftAnnex gitRepo
