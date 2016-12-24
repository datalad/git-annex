{- git-annex assistant repo syncing
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Sync where

import Assistant.Common
import Assistant.Pushes
import Assistant.Alert
import Assistant.Alert.Utility
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.RemoteControl
import qualified Command.Sync
import Utility.Parallel
import qualified Git
import qualified Git.Command
import qualified Git.Merge
import qualified Git.Ref
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.List as Remote
import qualified Annex.Branch
import Annex.UUID
import Annex.TaggedPush
import Annex.Ssh
import qualified Config
import Git.Config
import Assistant.NamedThread
import Assistant.Threads.Watcher (watchThread, WatcherControl(..))
import Assistant.TransferSlots
import Assistant.TransferQueue
import Assistant.RepoProblem
import Types.Transfer

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
 -
 - Also handles signaling any connectRemoteNotifiers, after the syncing is
 - done.
 -}
reconnectRemotes :: [Remote] -> Assistant ()
reconnectRemotes [] = noop
reconnectRemotes rs = void $ do
	rs' <- liftIO $ filterM (Remote.checkAvailable True) rs
	unless (null rs') $ do
		failedrs <- syncAction rs' (const go)
		forM_ failedrs $ \r ->
			whenM (liftIO $ Remote.checkAvailable False r) $
				repoHasProblem (Remote.uuid r) (syncRemote r)
		mapM_ signal $ filter (`notElem` failedrs) rs'
  where
	gitremotes = filter (notspecialremote . Remote.repo) rs
	(_xmppremotes, nonxmppremotes) = partition Remote.isXMPPRemote rs
	notspecialremote r
		| Git.repoIsUrl r = True
		| Git.repoIsLocal r = True
		| Git.repoIsLocalUnknown r = True
		| otherwise = False
	sync currentbranch@(Just _, _) = do
		(failedpull, diverged) <- manualPull currentbranch gitremotes
		now <- liftIO getCurrentTime
		failedpush <- pushToRemotes' now gitremotes
		return (nub $ failedpull ++ failedpush, diverged)
	{- No local branch exists yet, but we can try pulling. -}
	sync (Nothing, _) = manualPull (Nothing, Nothing) gitremotes
	go = do
		(failed, diverged) <- sync
			=<< liftAnnex (join Command.Sync.getCurrBranch)
		addScanRemotes diverged $
			filter (not . remoteAnnexIgnore . Remote.gitconfig)
				nonxmppremotes
		return failed
	signal r = liftIO . mapM_ (flip tryPutMVar ())
		=<< fromMaybe [] . M.lookup (Remote.uuid r) . connectRemoteNotifiers
			<$> getDaemonStatus

{- Pushes the local sync branch to all remotes, in
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
 - Returns any remotes that it failed to push to.
 -}
pushToRemotes :: [Remote] -> Assistant [Remote]
pushToRemotes remotes = do
	now <- liftIO getCurrentTime
	let remotes' = filter (not . remoteAnnexReadOnly . Remote.gitconfig) remotes
	syncAction remotes' (pushToRemotes' now)
pushToRemotes' :: UTCTime -> [Remote] -> Assistant [Remote]
pushToRemotes' now remotes = do
	(g, branch, u) <- liftAnnex $ do
		Annex.Branch.commit "update"
		(,,)
			<$> gitRepo
			<*> join Command.Sync.getCurrBranch
			<*> getUUID
	let (_xmppremotes, normalremotes) = partition Remote.isXMPPRemote remotes
	ret <- go True branch g u normalremotes
	return ret
  where
	go _ (Nothing, _) _ _ _ = return [] -- no branch, so nothing to do
	go _ _ _ _ [] = return [] -- no remotes, so nothing to do
	go shouldretry currbranch@(Just branch, _) g u rs =  do
		debug ["pushing to", show rs]
		(succeeded, failed) <- parallelPush g rs (push branch)
		updatemap succeeded []
		if null failed
			then return []
			else if shouldretry
				then retry currbranch g u failed
				else fallback branch g u failed

	updatemap succeeded failed = changeFailedPushMap $ \m ->
		M.union (makemap failed) $
			M.difference m (makemap succeeded)
	makemap l = M.fromList $ zip l (repeat now)

	retry currbranch g u rs = do
		debug ["trying manual pull to resolve failed pushes"]
		void $ manualPull currbranch rs
		go False currbranch g u rs

	fallback branch g u rs = do
		debug ["fallback pushing to", show rs]
		(succeeded, failed) <- parallelPush g rs (taggedPush u Nothing branch)
		updatemap succeeded failed
		return failed
		
	push branch remote = Command.Sync.pushBranch remote branch

parallelPush :: Git.Repo -> [Remote] -> (Remote -> Git.Repo -> IO Bool)-> Assistant ([Remote], [Remote])
parallelPush g rs a = do
	rgs <- liftAnnex $ mapM topush rs
	(succeededrgs, failedrgs) <- liftIO $ inParallel (uncurry a) rgs
	return (map fst succeededrgs, map fst failedrgs)
  where
	topush r = (,)
		<$> pure r
		<*> sshOptionsTo (Remote.repo r) (Remote.gitconfig r) g

{- Displays an alert while running an action that syncs with some remotes,
 - and returns any remotes that it failed to sync with.
 -
 - Readonly remotes are also hidden (to hide the web special remote).
 -}
syncAction :: [Remote] -> ([Remote] -> Assistant [Remote]) -> Assistant [Remote]
syncAction rs a
	| null visibleremotes = a rs
	| otherwise = do
		i <- addAlert $ syncAlert visibleremotes
		failed <- a rs
		let failed' = filter (not . Git.repoIsLocalUnknown . Remote.repo) failed
		let succeeded = filter (`notElem` failed) visibleremotes
		if null succeeded && null failed'
			then removeAlert i
			else updateAlertMap $ mergeAlert i $
				syncResultAlert succeeded failed'
		return failed
  where
	visibleremotes = filter (not . Remote.readonly) $
		filter (not . Remote.isXMPPRemote) rs

{- Manually pull from remotes and merge their branches. Returns any
 - remotes that it failed to pull from, and a Bool indicating
 - whether the git-annex branches of the remotes and local had
 - diverged before the pull.
 -}
manualPull :: Command.Sync.CurrBranch -> [Remote] -> Assistant ([Remote], Bool)
manualPull currentbranch remotes = do
	g <- liftAnnex gitRepo
	let (_xmppremotes, normalremotes) = partition Remote.isXMPPRemote remotes
	failed <- forM normalremotes $ \r -> do
		g' <- liftAnnex $ sshOptionsTo (Remote.repo r) (Remote.gitconfig r) g
		ifM (liftIO $ Git.Command.runBool [Param "fetch", Param $ Remote.name r] g')
			( return Nothing
			, return $ Just r
			)
	haddiverged <- liftAnnex Annex.Branch.forceUpdate
	forM_ normalremotes $ \r ->
		liftAnnex $ Command.Sync.mergeRemote r currentbranch Command.Sync.mergeConfig
	return (catMaybes failed, haddiverged)

{- Start syncing a remote, using a background thread. -}
syncRemote :: Remote -> Assistant ()
syncRemote remote = do
	updateSyncRemotes
	thread <- asIO $ do
		reconnectRemotes [remote]
		addScanRemotes True [remote]
	void $ liftIO $ forkIO $ thread

{- Use Nothing to change autocommit setting; or a remote to change
 - its sync setting. -}
changeSyncable :: Maybe Remote -> Bool -> Assistant ()
changeSyncable Nothing enable = do
	liftAnnex $ Config.setConfig key (boolConfig enable)
	liftIO . maybe noop (`throwTo` signal)
		=<< namedThreadId watchThread
  where
	key = Config.annexConfig "autocommit"
	signal
		| enable = ResumeWatcher
		| otherwise = PauseWatcher
changeSyncable (Just r) True = do
	liftAnnex $ changeSyncFlag r True
	syncRemote r
	sendRemoteControl RELOAD
changeSyncable (Just r) False = do
	liftAnnex $ changeSyncFlag r False
	updateSyncRemotes
	{- Stop all transfers to or from this remote.
	 - XXX Can't stop any ongoing scan, or git syncs. -}
	void $ dequeueTransfers tofrom
	mapM_ (cancelTransfer False) =<<
		filter tofrom . M.keys . currentTransfers <$> getDaemonStatus
  where
	tofrom t = transferUUID t == Remote.uuid r

changeSyncFlag :: Remote -> Bool -> Annex ()
changeSyncFlag r enabled = do
	Config.setConfig key (boolConfig enabled)
	void Remote.remoteListRefresh
  where
	key = Config.remoteConfig (Remote.repo r) "sync"
