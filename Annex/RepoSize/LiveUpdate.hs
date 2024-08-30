{- git-annex repo sizes, live updates
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.RepoSize.LiveUpdate where

import Annex.Common
import Logs.Presence.Pure
import Database.RepoSize.Handle
import Annex.UUID
import Types.FileMatcher
import Annex.LockFile
import Annex.LockPool
import qualified Database.RepoSize as Db
import qualified Utility.Matcher as Matcher

import Control.Concurrent
import System.Process
import Text.Read
import Data.Time.Clock.POSIX
import qualified Utility.RawFilePath as R
import qualified System.FilePath.ByteString as P

{- Called when a location log change is journalled, so the LiveUpdate
 - is done. This is called with the journal still locked, so no concurrent
 - changes can happen while it's running. Waits for the database
 - to be updated. -}
updateRepoSize :: LiveUpdate -> UUID -> Key -> LogStatus -> Annex ()
updateRepoSize lu u k s = liftIO $ finishedLiveUpdate lu u k sc
  where
	sc = case s of
		InfoPresent -> AddingKey
		InfoMissing -> RemovingKey
		InfoDead -> RemovingKey

-- When the UUID is Nothing, it's a live update of the local repository.
prepareLiveUpdate :: Maybe UUID -> Key -> SizeChange -> Annex LiveUpdate
prepareLiveUpdate mu k sc = do
	h <- Db.getRepoSizeHandle
	u <- maybe getUUID pure mu
	needv <- liftIO newEmptyMVar
	startv <- liftIO newEmptyMVar
	readyv <- liftIO newEmptyMVar
	donev <- liftIO newEmptyMVar
	void $ liftIO $ forkIO $ waitstart startv readyv donev h u
	return (LiveUpdate needv startv readyv donev)
  where
	{- Wait for checkLiveUpdate to request a start, or for the
	 - LiveUpdate to get garbage collected in the case where
	 - it is not needed.
	 -
	 - Deferring updating the database until here avoids overhead
	 - except in cases where preferred content expressions
	 - need live updates.
	 -}
	waitstart startv readyv donev h u =
		tryNonAsync (takeMVar startv) >>= \case
			Right () -> do
				pid <- getCurrentPid
				cid <- mkSizeChangeId pid
				Db.startingLiveSizeChange h u k sc cid
				putMVar readyv ()
				waitdone donev h u cid
			Left _ -> noop
	
	{- Wait for finishedLiveUpdate to be called, or for the LiveUpdate
	 - to get garbage collected in the case where the change didn't
	 - actually happen. Updates the database. -}
	waitdone donev h u cid = tryNonAsync (takeMVar donev) >>= \case
		Right (Just (u', k', sc', finishv))
			| u' == u && k' == k && sc' == sc -> do
				Db.successfullyFinishedLiveSizeChange h u k sc cid
				putMVar finishv ()
			-- Not the update we were expecting. This can
			-- happen when eg, storing to a cluster
			-- causes fanout and so this is called with
			-- other UUIDs.
			| otherwise -> do
				putMVar finishv ()
				waitdone donev h u cid
		Right Nothing -> abandoned h u cid
		Left _ -> abandoned h u cid
	abandoned h u cid = Db.removeStaleLiveSizeChange h u k sc cid

-- Called when a preferred content check indicates that a live update is
-- needed. Can be called more than once on the same LiveUpdate.
needLiveUpdate :: LiveUpdate -> Annex ()
needLiveUpdate NoLiveUpdate = noop
needLiveUpdate lu = liftIO $ void $ tryPutMVar (liveUpdateNeeded lu) ()

-- needLiveUpdate has to be called inside this to take effect. If the
-- action calls needLiveUpdate and then returns True, the live update is
-- started. If the action calls needLiveUpdate and then returns False,
-- the live update is not started.
--
-- This can be called more than once on the same LiveUpdate. It will
-- only start it once.
--
-- This serializes calls to the action, so that if the action
-- queries getLiveRepoSizes it will not race with another such action
-- that may also be starting a live update.
checkLiveUpdate
	:: LiveUpdate
	-> Matcher.Matcher (MatchFiles Annex)
	-> Annex Bool
	-> Annex Bool
checkLiveUpdate NoLiveUpdate _ a = a
checkLiveUpdate lu matcher a
	| Matcher.introspect matchNeedsLiveRepoSize matcher =
		Db.lockDbWhile (const go) go
	| otherwise = a
  where
	go = do
		r <- a
		needed <- liftIO $ isJust <$> tryTakeMVar (liveUpdateNeeded lu)
		when (r && needed) $ do
			liftIO $ void $ tryPutMVar (liveUpdateStart lu) ()
			liftIO $ void $ readMVar (liveUpdateReady lu)
		return r

finishedLiveUpdate :: LiveUpdate -> UUID -> Key -> SizeChange -> IO ()
finishedLiveUpdate NoLiveUpdate _ _ _ = noop
finishedLiveUpdate lu u k sc =
	whenM (not <$> isEmptyMVar (liveUpdateReady lu)) $ do
		finishv <- newEmptyMVar
		tryNonAsync (putMVar (liveUpdateDone lu) (Just (u, k, sc, finishv))) >>= \case
			Right () -> void $ tryNonAsync $ takeMVar finishv
			Left _ -> noop

{- Checks for other git-annex processes that might have been interrupted
 - and left the database populated with stale live size changes. Those
 - are removed from the database. 
 -
 - Also registers the current process so that other calls to this will not
 - consider it stale while it's running.
 -
 - This checks the first time it is called, and again if it's been more
 - than 1 minute since the last check.
 -}
checkStaleSizeChanges :: RepoSizeHandle -> Annex ()
checkStaleSizeChanges h@(RepoSizeHandle (Just _) livev) = do
	livedir <- calcRepo' gitAnnexRepoSizeLiveDir
	pid <- liftIO getCurrentPid
	let pidlockfile = show pid
	now <- liftIO getPOSIXTime
	liftIO (takeMVar livev) >>= \case
		Nothing -> do
			lck <- takeExclusiveLock $
				livedir P.</> toRawFilePath pidlockfile
			go livedir lck pidlockfile now
		Just v@(lck, lastcheck)
			| now >= lastcheck + 60 ->
				go livedir lck pidlockfile now
			| otherwise ->
				liftIO $ putMVar livev (Just v)
  where
	go livedir lck pidlockfile now = do
		void $ tryNonAsync $ do
			lockfiles <- liftIO $ filter (not . dirCruft) 
				<$> getDirectoryContents (fromRawFilePath livedir)
			stale <- forM lockfiles $ \lockfile ->
				if (lockfile /= pidlockfile)
					then case readMaybe lockfile of
						Nothing -> return Nothing
						Just pid -> checkstale livedir lockfile pid
					else return Nothing
			let stale' = catMaybes stale
			unless (null stale') $ liftIO $ do
				Db.removeStaleLiveSizeChanges h (map fst stale')
				mapM_ snd stale'
		liftIO $ putMVar livev (Just (lck, now))

	checkstale livedir lockfile pid =
		let f = livedir P.</> toRawFilePath lockfile
		in trySharedLock f >>= \case
			Nothing -> return Nothing
			Just lck -> do
				return $ Just 
					( StaleSizeChanger (SizeChangeProcessId pid)
					, do
						dropLock lck
						removeWhenExistsWith R.removeLink f
					)
checkStaleSizeChanges (RepoSizeHandle Nothing _) = noop
