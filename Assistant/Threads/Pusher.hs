{- git-annex assistant git pushing thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Pusher where

import Assistant.Common
import Assistant.Commits
import Assistant.Pushes
import Assistant.Alert
import Assistant.ThreadedMonad
import Assistant.Threads.Merger
import Assistant.DaemonStatus
import qualified Command.Sync
import Utility.ThreadScheduler
import Utility.Parallel

import Data.Time.Clock
import qualified Data.Map as M

thisThread :: ThreadName
thisThread = "Pusher"

{- This thread retries pushes that failed before. -}
pushRetryThread :: ThreadState -> DaemonStatusHandle -> FailedPushMap -> IO ()
pushRetryThread st dstatus pushmap = runEvery (Seconds halfhour) $ do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	topush <- getFailedPushesBefore pushmap (fromIntegral halfhour)
	unless (null topush) $ do
		debug thisThread
			[ "retrying"
			, show (length topush)
			, "failed pushes"
			]
		now <- getCurrentTime
		void $ alertWhile dstatus (pushRetryAlert topush) $
			pushToRemotes thisThread now st (Just pushmap) topush
	where
		halfhour = 1800

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: ThreadState -> DaemonStatusHandle -> CommitChan -> FailedPushMap -> IO ()
pushThread st dstatus commitchan pushmap = do
	runEvery (Seconds 2) $ do
		-- We already waited two seconds as a simple rate limiter.
		-- Next, wait until at least one commit has been made
		commits <- getCommits commitchan
		-- Now see if now's a good time to push.
		now <- getCurrentTime
		if shouldPush now commits
			then do
				remotes <- knownRemotes <$> getDaemonStatus dstatus
				void $ alertWhile dstatus (pushAlert remotes) $
					pushToRemotes thisThread now st (Just pushmap) remotes
			else do
				debug thisThread
					[ "delaying push of"
					, show (length commits)
					, "commits"
					]
				refillCommits commitchan commits

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
pushToRemotes :: ThreadName -> UTCTime -> ThreadState -> (Maybe FailedPushMap) -> [Remote] -> IO Bool
pushToRemotes threadname now st mpushmap remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> Command.Sync.currentBranch
	go True branch g remotes
	where
		go shouldretry branch g rs = do
			debug threadname
				[ "pushing to"
				, show rs
				]
			Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
			{- TODO git push exits nonzero if the remote
			 - is already up-to-date. This code does not tell
			 - the difference between the two. Could perhaps
			 - be check the refs when it seemed to fail?
			 - Note bewloe  -}
			(succeeded, failed) <- inParallel (push g branch) rs
			case mpushmap of
				Nothing -> noop
				Just pushmap -> 
					changeFailedPushMap pushmap $ \m ->
						M.union (makemap failed) $
							M.difference m (makemap succeeded)
			unless (null failed) $
				debug threadname
					[ "failed to push to"
					, show failed
					]
			if (null failed || not shouldretry)
				{- TODO see above TODO item -}
				then return True -- return $ null failed 
				else retry branch g failed

		makemap l = M.fromList $ zip l (repeat now)

		push g branch remote =
			ifM (Command.Sync.pushBranch remote branch g)
				( exitSuccess, exitFailure)

		retry branch g rs = do
			debug threadname [ "trying manual pull to resolve failed pushes" ]
			runThreadState st $ manualPull branch rs
			go False branch g rs
