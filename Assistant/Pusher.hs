{- git-annex assistant git pushing thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pusher where

import Common.Annex
import Assistant.Commits
import Assistant.ThreadedMonad
import qualified Command.Sync
import Utility.ThreadScheduler
import Utility.Parallel

import Data.Time.Clock

data FailedPush = FailedPush 
	{ failedRemote :: Remote
	, failedTimeStamp :: UTCTime
	}

{- This thread pushes git commits out to remotes. -}
pushThread :: ThreadState -> CommitChan -> IO ()
pushThread st commitchan = do
	remotes <- runThreadState st $ Command.Sync.syncRemotes []
	runEveryWith (Seconds 2) [] $ \failedpushes -> do
		-- We already waited two seconds as a simple rate limiter.
		-- Next, wait until at least one commit has been made
		commits <- getCommits commitchan
		-- Now see if now's a good time to push.
		time <- getCurrentTime
		if shouldPush time commits failedpushes
			then pushToRemotes time st remotes
			else do
				refillCommits commitchan commits
				return failedpushes

{- Decide if now is a good time to push to remotes.
 -
 - Current strategy: Immediately push all commits. The commit machinery
 - already determines batches of changes, so we can't easily determine
 - batches better.
 -
 - TODO: FailedPushs are only retried the next time there's a commit.
 - Should retry them periodically, or when a remote that was not available
 - becomes available.
 -}
shouldPush :: UTCTime -> [Commit] -> [FailedPush] -> Bool
shouldPush _now commits _failedremotes
	| not (null commits) = True
	| otherwise = False

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel.
 -
 - Avoids running possibly long-duration commands in the Annex monad, so
 - as not to block other threads. -}
pushToRemotes :: UTCTime -> ThreadState -> [Remote] -> IO [FailedPush]
pushToRemotes now st remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> Command.Sync.currentBranch
	Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
	map (`FailedPush` now) <$> inParallel (push g branch) remotes
	where
		push g branch remote =
			ifM (Command.Sync.pushBranch remote branch g)
				( exitSuccess, exitFailure)
