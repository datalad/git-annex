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
import qualified Annex.Branch

import Data.Time.Clock
import qualified Data.Map as M

{- Syncs with remotes that may have been disconnected for a while.
 - 
 - After getting git in sync, queues a scan for file transfers.
 -}
syncRemotes :: ThreadName -> ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> [Remote] -> IO ()
syncRemotes _ _ _ _ [] = noop
syncRemotes threadname st dstatus scanremotes rs = do
	void $ alertWhile dstatus (syncAlert rs) $ do
		sync =<< runThreadState st (inRepo Git.Branch.current)
	addScanRemotes scanremotes rs
	where
		sync (Just branch) = do
			runThreadState st $ manualPull (Just branch) rs
			now <- getCurrentTime	
			pushToRemotes threadname now st Nothing rs
		{- No local branch exists yet, but we can try pulling. -}
		sync Nothing = do
			runThreadState st $ manualPull Nothing rs
			return True

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
			runThreadState st $ manualPull (Just branch) rs
			go False (Just branch) g rs

{- Manually pull from remotes and merge their branches. -}
manualPull :: (Maybe Git.Ref) -> [Remote] -> Annex ()
manualPull currentbranch remotes = do
	forM_ remotes $ \r ->
		inRepo $ Git.Command.runBool "fetch" [Param $ Remote.name r]
	Annex.Branch.forceUpdate
	forM_ remotes $ \r ->
		Command.Sync.mergeRemote r currentbranch
