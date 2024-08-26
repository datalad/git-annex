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
import qualified Database.RepoSize as Db
import Annex.UUID

import Control.Concurrent
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Process

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

addKeyRepoSize :: Key -> Maybe RepoSize -> Maybe RepoSize
addKeyRepoSize k mrs = case mrs of
	Just (RepoSize sz) -> Just $ RepoSize $ sz + ksz
	Nothing -> Just $ RepoSize ksz
  where
	ksz = fromMaybe 0 $ fromKey keySize k

removeKeyRepoSize :: Key -> Maybe RepoSize -> Maybe RepoSize
removeKeyRepoSize k mrs = case mrs of
	Just (RepoSize sz) -> Just $ RepoSize $ sz - ksz
	Nothing -> Nothing
  where
	ksz = fromMaybe 0 $ fromKey keySize k

accumRepoSizes :: Key -> (S.Set UUID, S.Set UUID) -> M.Map UUID RepoSize -> M.Map UUID RepoSize
accumRepoSizes k (newlocs, removedlocs) sizemap = 
	let !sizemap' = foldl' (flip $ M.alter $ addKeyRepoSize k) sizemap newlocs
	in foldl' (flip $ M.alter $ removeKeyRepoSize k) sizemap' removedlocs

-- When the UUID is Nothing, it's a live update of the local repository.
prepareLiveUpdate :: Maybe UUID -> Key -> SizeChange -> Annex LiveUpdate
prepareLiveUpdate mu k sc = do
	h <- Db.getRepoSizeHandle
	u <- maybe getUUID pure mu
	needv <- liftIO newEmptyMVar
	startv <- liftIO newEmptyMVar
	readyv <- liftIO newEmptyMVar
	donev <- liftIO newEmptyMVar
	finishv <- liftIO newEmptyMVar
	void $ liftIO $ forkIO $ waitstart startv readyv donev finishv h u
	return (LiveUpdate needv startv readyv donev finishv)
  where
	{- Wait for checkLiveUpdate to request a start, or for the
	 - LiveUpdate to get garbage collected in the case where
	 - it is not needed. -}
	waitstart startv readyv donev finishv h u =
		tryNonAsync (takeMVar startv) >>= \case
			Right () -> do
				pid <- getCurrentPid
				cid <- mkSizeChangeId pid
				{- Deferring updating the database until
				 - here avoids overhead except in cases
				 - where preferred content expressions
				 - need live updates. -}
				Db.startingLiveSizeChange h u k sc cid
				putMVar readyv ()
				waitdone donev finishv h u cid
			Left _ -> noop
	
	{- Wait for finishedLiveUpdate to be called, or for the LiveUpdate
	 - to get garbage collected in the case where the change didn't
	 - actually happen. Updates the database. -}
	waitdone donev finishv h u cid = tryNonAsync (takeMVar donev) >>= \case
		Right (Just (u', k', sc'))
			| u' == u && k' == k && sc' == sc -> do
				Db.successfullyFinishedLiveSizeChange h u k sc cid
				putMVar finishv ()
			-- This can happen when eg, storing to a cluster
			-- causes fanout and so this is called with
			-- other UUIDs.
			| otherwise -> waitdone donev finishv h u cid
		Right Nothing -> abandoned h u cid
		Left _ -> abandoned h u cid
	abandoned h u cid = Db.staleLiveSizeChange h u k sc cid

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
checkLiveUpdate :: LiveUpdate -> Annex Bool -> Annex Bool
checkLiveUpdate NoLiveUpdate a = a
checkLiveUpdate lu a = do
	r <- a
	needed <- liftIO $ isJust <$> tryTakeMVar (liveUpdateNeeded lu)
	when (r && needed) $ do
		liftIO $ void $ tryPutMVar (liveUpdateStart lu) ()
		liftIO $ void $ readMVar (liveUpdateReady lu)
	return r

finishedLiveUpdate :: LiveUpdate -> UUID -> Key -> SizeChange -> IO ()
finishedLiveUpdate NoLiveUpdate _ _ _ = noop
finishedLiveUpdate lu u k sc = do
	tryNonAsync (putMVar (liveUpdateDone lu) (Just (u, k, sc))) >>= \case
		Right () -> void $
			tryNonAsync $ readMVar $ liveUpdateFinish lu
		Left _ -> noop
