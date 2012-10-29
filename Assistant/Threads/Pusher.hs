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
import Assistant.DaemonStatus
import Assistant.Sync
import Utility.ThreadScheduler
import qualified Remote
import qualified Types.Remote as Remote

import Data.Time.Clock

thisThread :: ThreadName
thisThread = "Pusher"

{- This thread retries pushes that failed before. -}
pushRetryThread :: NamedThread
pushRetryThread = NamedThread "PushRetrier" $ runEvery (Seconds halfhour) <~> do
	-- We already waited half an hour, now wait until there are failed
	-- pushes to retry.
	pushmap <- getAssistant failedPushMap
	topush <- liftIO $ getFailedPushesBefore pushmap (fromIntegral halfhour)
	unless (null topush) $ do
		debug ["retrying", show (length topush), "failed pushes"]
		now <- liftIO $ getCurrentTime
		st <- getAssistant threadState
		pushnotifier <- getAssistant pushNotifier
		dstatus <- getAssistant daemonStatusHandle
		void $ liftIO $ alertWhile dstatus (pushRetryAlert topush) $
			pushToRemotes thisThread now st (Just pushnotifier) (Just pushmap) topush
	where
		halfhour = 1800

{- This thread pushes git commits out to remotes soon after they are made. -}
pushThread :: NamedThread
pushThread = NamedThread "Pusher" $ runEvery (Seconds 2) <~> do
	-- We already waited two seconds as a simple rate limiter.
	-- Next, wait until at least one commit has been made
	commits <- getCommits <<~ commitChan
	-- Now see if now's a good time to push.
	if shouldPush commits
		then do
			remotes <- filter pushable . syncRemotes <$> daemonStatus
			unless (null remotes) $ do
				now <- liftIO $ getCurrentTime
				st <- getAssistant threadState
				pushmap <- getAssistant failedPushMap
				pushnotifier <- getAssistant pushNotifier		
				dstatus <- getAssistant daemonStatusHandle
				void $ liftIO $ alertWhile dstatus (pushAlert remotes) $
					pushToRemotes thisThread now st (Just pushnotifier) (Just pushmap) remotes
		else do
			debug ["delaying push of", show (length commits), "commits"]
			flip refillCommits commits <<~ commitChan
  where
	pushable r
		| Remote.specialRemote r = False
		| Remote.readonly r = False
		| otherwise = True

{- Decide if now is a good time to push to remotes.
 -
 - Current strategy: Immediately push all commits. The commit machinery
 - already determines batches of changes, so we can't easily determine
 - batches better.
 -}
shouldPush :: [Commit] -> Bool
shouldPush commits
	| not (null commits) = True
	| otherwise = False
