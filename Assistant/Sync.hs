{- git-annex assistant repo syncing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Sync where

import Assistant.Common
import Assistant.Pushes
import Assistant.Alert
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import qualified Command.Sync
import Utility.Parallel
import qualified Git
import qualified Git.Branch
import qualified Git.Command
import qualified Remote
import qualified Types.Remote as Remote
import qualified Annex.Branch

import Data.Time.Clock
import qualified Data.Map as M

{- Syncs with remotes that may have been disconnected for a while.
 - 
 - First gets git in sync, and then prepares any necessary file transfers.
 -
 - An expensive full scan is queued when the git-annex branches of some of
 - the remotes have diverged from the local git-annex branch. Otherwise,
 - it's sufficient to requeue failed transfers.
 -}
reconnectRemotes :: ThreadName -> ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> [Remote] -> IO ()
reconnectRemotes _ _ _ _ [] = noop
reconnectRemotes threadname st dstatus scanremotes rs = void $
	alertWhile dstatus (syncAlert rs) $ do
		(ok, diverged) <- sync
			=<< runThreadState st (inRepo Git.Branch.current)
		addScanRemotes scanremotes diverged rs
		return ok
	where
		(gitremotes, specialremotes) =
			partition (Git.repoIsUrl . Remote.repo) rs
		sync (Just branch) = do
			diverged <- manualPull st (Just branch) gitremotes
			now <- getCurrentTime
			ok <- pushToRemotes threadname now st Nothing gitremotes
			return (ok, diverged)
		{- No local branch exists yet, but we can try pulling. -}
		sync Nothing = do
			diverged <- manualPull st Nothing gitremotes
			return (True, diverged)

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel.
 -
 - Avoids running possibly long-duration commands in the Annex monad, so
 - as not to block other threads. -}
pushToRemotes :: ThreadName -> UTCTime -> ThreadState -> (Maybe FailedPushMap) -> [Remote] -> IO Bool
pushToRemotes threadname now st mpushmap remotes = do
	(g, branch) <- runThreadState st $
		(,) <$> fromRepo id <*> inRepo Git.Branch.current
	go True branch g remotes
	where
		go _ Nothing _ _ = return True -- no branch, so nothing to do
		go shouldretry (Just branch) g rs = do
			debug threadname
				[ "pushing to"
				, show rs
				]
			Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
			(succeeded, failed) <- inParallel (push g branch) rs
			let ok = null failed
			case mpushmap of
				Nothing -> noop
				Just pushmap -> 
					changeFailedPushMap pushmap $ \m ->
						M.union (makemap failed) $
							M.difference m (makemap succeeded)
			unless (ok) $
				debug threadname
					[ "failed to push to"
					, show failed
					]
			if (ok || not shouldretry)
				then return ok
				else retry branch g failed

		makemap l = M.fromList $ zip l (repeat now)

		push g branch remote = Command.Sync.pushBranch remote branch g

		retry branch g rs = do
			debug threadname [ "trying manual pull to resolve failed pushes" ]
			void $ manualPull st (Just branch) rs
			go False (Just branch) g rs

{- Manually pull from remotes and merge their branches. -}
manualPull :: ThreadState -> (Maybe Git.Ref) -> [Remote] -> IO Bool
manualPull st currentbranch remotes = do
	g <- runThreadState st $ fromRepo id
	forM_ remotes $ \r ->
		Git.Command.runBool "fetch" [Param $ Remote.name r] g
	haddiverged <- runThreadState st $ Annex.Branch.forceUpdate
	forM_ remotes $ \r ->
		runThreadState st $ Command.Sync.mergeRemote r currentbranch
	return haddiverged
