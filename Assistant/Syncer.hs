{- git-annex assistant git syncing thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.Syncer where

import Common.Annex
import Assistant.Commits
import Assistant.ThreadedMonad
import qualified Command.Sync
import Utility.ThreadScheduler
import Utility.Parallel

import Data.Time.Clock

data FailedSync = FailedSync 
	{ failedRemote :: Remote
	, failedTimeStamp :: UTCTime
	}

{- This thread syncs git commits out to remotes. -}
syncThread :: ThreadState -> CommitChan -> IO ()
syncThread st commitchan = do
	remotes <- runThreadState st $ Command.Sync.syncRemotes []
	runEveryWith (Seconds 2) [] $ \failedsyncs -> do
		-- We already waited two seconds as a simple rate limiter.
		-- Next, wait until at least one commit has been made
		commits <- getCommits commitchan
		-- Now see if now's a good time to sync.
		time <- getCurrentTime
		if shouldSync time commits failedsyncs
			then syncToRemotes time st remotes
			else do
				refillCommits commitchan commits
				return failedsyncs

{- Decide if now is a good time to sync to remotes.
 -
 - Current strategy: Immediately sync all commits. The commit machinery
 - already determines batches of changes, so we can't easily determine
 - batches better.
 -
 - TODO: FailedSyncs are only retried the next time there's a commit.
 - Should retry them periodically, or when a remote that was not available
 - becomes available.
 -}
shouldSync :: UTCTime -> [Commit] -> [FailedSync] -> Bool
shouldSync _now commits _failedremotes
	| not (null commits) = True
	| otherwise = False

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel.
 -
 - Avoids running possibly long-duration commands in the Annex monad, so
 - as not to block other threads. -}
syncToRemotes :: UTCTime -> ThreadState -> [Remote] -> IO [FailedSync]
syncToRemotes now st remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> Command.Sync.currentBranch
	Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
	map (`FailedSync` now) <$> inParallel (go g branch) remotes
	where
		go g branch remote =
			ifM (Command.Sync.pushBranch remote branch g)
				( exitSuccess, exitFailure)
