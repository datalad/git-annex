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
import qualified Command.Sync
import Utility.ThreadScheduler
import Utility.Parallel

import Data.Time.Clock

{- This thread retries pushes that failed before. -}
pushRetryThread :: ThreadState -> FailedPushChan -> IO ()
pushRetryThread st pushchan = runEvery (Seconds halfhour) $ do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	pushes <- getFailedPushes pushchan
	-- Check times, to avoid repushing a push that's too new.
	now <- getCurrentTime
	let (newpushes, oldpushes) = partition (toorecent now . failedTimeStamp) pushes
	unless (null newpushes) $
		refillFailedPushes pushchan newpushes
	unless (null oldpushes) $
		pushToRemotes now st pushchan $ map failedRemote oldpushes
	where
		halfhour = 1800
		toorecent now time = now `diffUTCTime` time < fromIntegral halfhour

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: ThreadState -> CommitChan -> FailedPushChan -> IO ()
pushThread st commitchan pushchan = do
	remotes <- runThreadState st $ Command.Sync.syncRemotes []
	runEvery (Seconds 2) $ do
		-- We already waited two seconds as a simple rate limiter.
		-- Next, wait until at least one commit has been made
		commits <- getCommits commitchan
		-- Now see if now's a good time to push.
		now <- getCurrentTime
		if shouldPush now commits
			then pushToRemotes now st pushchan remotes
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
pushToRemotes :: UTCTime -> ThreadState -> FailedPushChan -> [Remote] -> IO ()
pushToRemotes now st pushchan remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> Command.Sync.currentBranch
	Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
	failed <- map (`FailedPush` now) <$> inParallel (push g branch) remotes
	unless (null failed) $
		refillFailedPushes pushchan failed
	where
		push g branch remote =
			ifM (Command.Sync.pushBranch remote branch g)
				( exitSuccess, exitFailure)
