{- git-annex assistant git pushing threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Pusher where

import Common.Annex
import Assistant.Commits
import Assistant.Pushes
import Assistant.ThreadedMonad
import Assistant.Threads.Merger
import qualified Command.Sync
import Utility.ThreadScheduler
import Utility.Parallel

import Data.Time.Clock
import qualified Data.Map as M

{- This thread retries pushes that failed before. -}
pushRetryThread :: ThreadState -> FailedPushMap -> IO ()
pushRetryThread st pushmap = runEvery (Seconds halfhour) $ do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	topush <- getFailedPushesBefore pushmap (fromIntegral halfhour)
	unless (null topush) $ do
		now <- getCurrentTime
		pushToRemotes now st pushmap topush
	where
		halfhour = 1800

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: ThreadState -> CommitChan -> FailedPushMap -> IO ()
pushThread st commitchan pushmap = do
	remotes <- runThreadState st $ Command.Sync.syncRemotes []
	runEvery (Seconds 2) $ do
		-- We already waited two seconds as a simple rate limiter.
		-- Next, wait until at least one commit has been made
		commits <- getCommits commitchan
		-- Now see if now's a good time to push.
		now <- getCurrentTime
		if shouldPush now commits
			then pushToRemotes now st pushmap remotes
			else refillCommits commitchan commits

{- Decide if now is a good time to push to remotes.
 -
 - Current strategy: Immediately push all commits. The commit machinery
 - already determines batches of changes, so we can't easily determine
 - batches better.
 -}
shouldPush :: UTCTime -> [Commit] -> Bool
shouldPush _now commits
	| not (null commits) = True
	| otherwise = False

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel.
 -
 - Avoids running possibly long-duration commands in the Annex monad, so
 - as not to block other threads. -}
pushToRemotes :: UTCTime -> ThreadState -> FailedPushMap -> [Remote] -> IO ()
pushToRemotes now st pushmap remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> Command.Sync.currentBranch
	go True branch g remotes
	where
		go shouldretry branch g rs = do
			Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
			(succeeded, failed) <- inParallel (push g branch) rs
			changeFailedPushMap pushmap $ \m ->
				M.union (makemap failed) $
					M.difference m (makemap succeeded)
			unless (null failed || not shouldretry) $
				retry branch g failed

		makemap l = M.fromList $ zip l (repeat now)

		push g branch remote =
			ifM (Command.Sync.pushBranch remote branch g)
				( exitSuccess, exitFailure)

		retry branch g rs = do
			runThreadState st $ manualPull branch rs
			go False branch g rs
