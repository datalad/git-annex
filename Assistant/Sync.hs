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
import qualified Git.Ref
import qualified Git.Command
import qualified Remote
import qualified Types.Remote as Remote
import qualified Annex.Branch
import Annex.UUID

import Data.Time.Clock
import qualified Data.Map as M
import Control.Concurrent

{- Syncs with remotes that may have been disconnected for a while.
 - 
 - First gets git in sync, and then prepares any necessary file transfers.
 -
 - An expensive full scan is queued when the git-annex branches of some of
 - the remotes have diverged from the local git-annex branch. Otherwise,
 - it's sufficient to requeue failed transfers.
 -}
reconnectRemotes :: ThreadName -> ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> Maybe PushNotifier -> [Remote] -> IO ()
reconnectRemotes _ _ _ _ _ [] = noop
reconnectRemotes threadname st dstatus scanremotes pushnotifier rs = void $
	alertWhile dstatus (syncAlert rs) $ do
		(ok, diverged) <- sync
			=<< runThreadState st (inRepo Git.Branch.current)
		addScanRemotes scanremotes diverged rs
		return ok
	where
		(gitremotes, _specialremotes) =
			partition (Git.repoIsUrl . Remote.repo) rs
		sync (Just branch) = do
			diverged <- snd <$> manualPull st (Just branch) gitremotes
			now <- getCurrentTime
			ok <- pushToRemotes threadname now st pushnotifier Nothing gitremotes
			return (ok, diverged)
		{- No local branch exists yet, but we can try pulling. -}
		sync Nothing = do
			diverged <- snd <$> manualPull st Nothing gitremotes
			return (True, diverged)

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel, along with the git-annex branch. This is the same
 - as "git annex sync", except in parallel, and will co-exist with use of
 - "git annex sync".
 -
 - Avoids running possibly long-duration commands in the Annex monad, so
 - as not to block other threads.
 -
 - This can fail, when the remote's sync branch (or git-annex branch) has
 - been updated by some other remote pushing into it, or by the remote
 - itself. To handle failure, a manual pull and merge is done, and the push
 - is retried.
 -
 - When there's a lot of activity, we may fail more than once.
 - On the other hand, we may fail because the remote is not available.
 - Rather than retrying indefinitely, after the first retry we enter a
 - fallback mode, where our push is guarenteed to succeed if the remote is
 - reachable. If the fallback fails, the push is queued to be retried
 - later.
 -
 - The fallback mode pushes to branches on the remote that have our uuid in
 - them. While ugly, those branches are reserved for pushing by us, and
 - so our pushes will succeed.
 -}
pushToRemotes :: ThreadName -> UTCTime -> ThreadState -> Maybe PushNotifier -> Maybe FailedPushMap -> [Remote] -> IO Bool
pushToRemotes threadname now st mpushnotifier mpushmap remotes = do
	(g, branch, u) <- runThreadState st $ do
		Annex.Branch.commit "update"
		(,,)
			<$> gitRepo
			<*> inRepo Git.Branch.current
			<*> getUUID
	go True branch g u remotes
	where
		go _ Nothing _ _ _ = return True -- no branch, so nothing to do
		go shouldretry (Just branch) g u rs =  do
			debug threadname
				[ "pushing to"
				, show rs
				]
			Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
			(succeeded, failed) <- inParallel (push g branch) rs
			updatemap succeeded []
			let ok = null failed
			if ok
				then do
					maybe noop (notifyPush $ map Remote.uuid succeeded) mpushnotifier
					return ok
				else if shouldretry
					then retry branch g u failed
					else fallback branch g u failed

		updatemap succeeded failed = case mpushmap of
			Nothing -> noop
			Just pushmap -> changeFailedPushMap pushmap $ \m ->
				M.union (makemap failed) $
					M.difference m (makemap succeeded)
		makemap l = M.fromList $ zip l (repeat now)

		retry branch g u rs = do
			debug threadname [ "trying manual pull to resolve failed pushes" ]
			void $ manualPull st (Just branch) rs
			go False (Just branch) g u rs

		fallback branch g u rs = do
			debug threadname
				[ "fallback pushing to"
				, show rs
				]
			(succeeded, failed) <- inParallel (pushfallback g u branch) rs
			updatemap succeeded failed
			unless (null succeeded) $
				maybe noop (notifyPush $ map Remote.uuid succeeded) mpushnotifier
			return $ null failed
			
		push g branch remote = Command.Sync.pushBranch remote branch g
		pushfallback g u branch remote = Git.Command.runBool "push"
			[ Param $ Remote.name remote
			, Param $ refspec Annex.Branch.name
			, Param $ refspec branch
			] g
			where
				{- Push to refs/synced/uuid/branch; this
				 - avoids cluttering up the branch display. -}
				refspec b = concat
					[ s
					, ":"
					, "refs/synced/" ++ fromUUID u ++ "/" ++ s
					]
					where s = show $ Git.Ref.base b

{- Manually pull from remotes and merge their branches. -}
manualPull :: ThreadState -> Maybe Git.Ref -> [Remote] -> IO ([Bool], Bool)
manualPull st currentbranch remotes = do
	g <- runThreadState st gitRepo
	results <- forM remotes $ \r ->
		Git.Command.runBool "fetch" [Param $ Remote.name r] g
	haddiverged <- runThreadState st Annex.Branch.forceUpdate
	forM_ remotes $ \r ->
		runThreadState st $ Command.Sync.mergeRemote r currentbranch
	return (results, haddiverged)

{- Start syncing a newly added remote, using a background thread. -}
syncNewRemote :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> Remote -> IO ()
syncNewRemote st dstatus scanremotes remote = do
	runThreadState st $ updateSyncRemotes dstatus
	void $ forkIO $ reconnectRemotes "SyncRemote" st dstatus scanremotes Nothing [remote]
