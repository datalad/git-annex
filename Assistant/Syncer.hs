{- git-annex assistant git syncing thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.Syncer where

import Assistant.Commits
import Assistant.ThreadedMonad
import qualified Command.Sync
import Utility.ThreadScheduler

{- This thread syncs git commits out to remotes. -}
syncThread :: ThreadState -> CommitChan -> IO ()
syncThread st commitchan = runEvery (Seconds 2) $ do
	-- We already waited two seconds as a simple rate limiter.
	-- Next, wait until at least one commit has been made
	commits <- getCommits commitchan
	-- Now see if now's a good time to sync.
	if shouldSync commits
		then syncToRemotes
		else refillCommits commitchan commits

{- Decide if now is a good time to sync commits to remotes. -}
shouldSync :: [Commit] -> Bool
shouldSync commits = not (null commits)

syncToRemotes :: IO ()
syncToRemotes = return () -- TOOD
