{- git-annex assistant git pushing thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Pusher where

import Assistant.Common
import Assistant.Commits
import Assistant.Pushes
import Assistant.DaemonStatus
import Assistant.Sync
import Utility.ThreadScheduler
import qualified Remote
import qualified Types.Remote as Remote

{- This thread retries pushes that failed before. -}
pushRetryThread :: NamedThread
pushRetryThread = namedThread "PushRetrier" $ runEvery (Seconds halfhour) <~> do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	topush <- getFailedPushesBefore (fromIntegral halfhour)
	unless (null topush) $ do
		debug ["retrying", show (length topush), "failed pushes"]
		void $ pushToRemotes topush
  where
	halfhour = 1800

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: NamedThread
pushThread = namedThread "Pusher" $ runEvery (Seconds 2) <~> do
	-- We already waited two seconds as a simple rate limiter.
	-- Next, wait until at least one commit has been made
	void getCommits
	-- Now see if now's a good time to push.
	void $ pushToRemotes =<< pushTargets

{- We want to avoid pushing to remotes that are marked readonly.
 -
 - Also, avoid pushing to local remotes we can easily tell are not available,
 - to avoid ugly messages when a removable drive is not attached.
 -}
pushTargets :: Assistant [Remote]
pushTargets = liftIO . filterM (Remote.checkAvailable True)
	=<< candidates <$> getDaemonStatus
  where
	candidates = filter (not . Remote.readonly) . syncGitRemotes
