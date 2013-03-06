{- git-annex assistant repo syncing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Sync where

import Assistant.Common
import Assistant.Pushes
import Assistant.NetMessager
import Assistant.Types.NetMessager
import Assistant.Alert
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
import Annex.UUID
import Annex.TaggedPush

import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent

{- Syncs with remotes that may have been disconnected for a while.
 - 
 - First gets git in sync, and then prepares any necessary file transfers.
 -
 - An expensive full scan is queued when the git-annex branches of some of
 - the remotes have diverged from the local git-annex branch. Otherwise,
 - it's sufficient to requeue failed transfers.
 -
 - XMPP remotes are also signaled that we can push to them, and we request
 - they push to us. Since XMPP pushes run ansynchronously, any scan of the
 - XMPP remotes has to be deferred until they're done pushing to us, so
 - all XMPP remotes are marked as possibly desynced.
 -}
reconnectRemotes :: Bool -> [Remote] -> Assistant ()
reconnectRemotes _ [] = noop
reconnectRemotes notifypushes rs = void $ do
	modifyDaemonStatus_ $ \s -> s
		{ desynced = S.union (S.fromList $ map Remote.uuid xmppremotes) (desynced s) }
	alertWhile (syncAlert normalremotes) $ do
		(ok, diverged) <- sync
			=<< liftAnnex (inRepo Git.Branch.current)
		addScanRemotes diverged rs
		return ok
  where
	gitremotes = filter (notspecialremote . Remote.repo) rs
	(xmppremotes, normalremotes) = partition isXMPPRemote gitremotes
	notspecialremote r
		| Git.repoIsUrl r = True
		| Git.repoIsLocal r = True
		| otherwise = False
	sync (Just branch) = do
		diverged <- snd <$> manualPull (Just branch) gitremotes
		now <- liftIO getCurrentTime
		ok <- pushToRemotes now notifypushes gitremotes
		return (ok, diverged)
	{- No local branch exists yet, but we can try pulling. -}
	sync Nothing = do
		diverged <- snd <$> manualPull Nothing gitremotes
		return (True, diverged)

{- Updates the local sync branch, then pushes it to all remotes, in
 - parallel, along with the git-annex branch. This is the same
 - as "git annex sync", except in parallel, and will co-exist with use of
 - "git annex sync".
 -
 - After the pushes to normal git remotes, also signals XMPP clients that
 - they can request an XMPP push.
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
 -}
pushToRemotes :: UTCTime -> Bool -> [Remote] -> Assistant Bool
pushToRemotes now notifypushes remotes = do
	(g, branch, u) <- liftAnnex $ do
		Annex.Branch.commit "update"
		(,,)
			<$> gitRepo
			<*> inRepo Git.Branch.current
			<*> getUUID
	let (xmppremotes, normalremotes) = partition isXMPPRemote remotes
	ret <- go True branch g u normalremotes
	forM_ xmppremotes $ \r ->
		sendNetMessage $ Pushing (getXMPPClientID r) CanPush
	return ret
  where
	go _ Nothing _ _ _ = return True -- no branch, so nothing to do
	go _ _ _ _ [] = return True -- no remotes, so nothing to do
	go shouldretry (Just branch) g u rs =  do
		debug ["pushing to", show rs]
		liftIO $ Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
		(succeeded, failed) <- liftIO $ inParallel (push g branch) rs
		updatemap succeeded []
		if null failed
			then do
				when notifypushes $
					sendNetMessage $ NotifyPush $
						map Remote.uuid succeeded
				return True
			else if shouldretry
				then retry branch g u failed
				else fallback branch g u failed

	updatemap succeeded failed = changeFailedPushMap $ \m ->
		M.union (makemap failed) $
			M.difference m (makemap succeeded)
	makemap l = M.fromList $ zip l (repeat now)

	retry branch g u rs = do
		debug ["trying manual pull to resolve failed pushes"]
		void $ manualPull (Just branch) rs
		go False (Just branch) g u rs

	fallback branch g u rs = do
		debug ["fallback pushing to", show rs]
		(succeeded, failed) <- liftIO $
			inParallel (\r -> taggedPush u branch r g) rs
		updatemap succeeded failed
		when (notifypushes && (not $ null succeeded)) $
			sendNetMessage $ NotifyPush $
				map Remote.uuid succeeded
		return $ null failed
		
	push g branch remote = Command.Sync.pushBranch remote branch g

{- Manually pull from remotes and merge their branches. Returns the results
 - of all the pulls, and whether the git-annex branches of the remotes and
 - local had divierged before the pull.
 -
 - After pulling from the normal git remotes, requests pushes from any XMPP
 - remotes. However, those pushes will run asynchronously, so their
 - results are not included in the return data.
 -}
manualPull :: Maybe Git.Ref -> [Remote] -> Assistant ([Bool], Bool)
manualPull currentbranch remotes = do
	g <- liftAnnex gitRepo
	let (xmppremotes, normalremotes) = partition isXMPPRemote remotes
	results <- liftIO $ forM normalremotes $ \r ->
		Git.Command.runBool [Param "fetch", Param $ Remote.name r] g
	haddiverged <- liftAnnex Annex.Branch.forceUpdate
	forM_ normalremotes $ \r ->
		liftAnnex $ Command.Sync.mergeRemote r currentbranch
	forM_ xmppremotes $ \r ->
		sendNetMessage $ Pushing (getXMPPClientID r) PushRequest
	return (results, haddiverged)

{- Start syncing a newly added remote, using a background thread. -}
syncNewRemote :: Remote -> Assistant ()
syncNewRemote remote = do
	updateSyncRemotes
	thread <- asIO $ do
		reconnectRemotes False [remote]
		addScanRemotes True [remote]
	void $ liftIO $ forkIO $ thread
