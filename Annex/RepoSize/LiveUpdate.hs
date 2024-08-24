{- git-annex repo sizes, live updates
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.RepoSize.LiveUpdate where

import Annex.Common
import qualified Annex
import Logs.Presence.Pure
import qualified Database.RepoSize as Db
import Annex.UUID

import Control.Concurrent
import qualified Data.Map.Strict as M
import qualified Data.Set as S

updateRepoSize :: LiveUpdate -> UUID -> Key -> LogStatus -> Annex ()
updateRepoSize lu u k s = do
	-- XXX call finishedLiveUpdate
	rsv <- Annex.getRead Annex.reposizes
	liftIO (takeMVar rsv) >>= \case
		Nothing -> liftIO (putMVar rsv Nothing)
		Just sizemap -> do
			let !sizemap' = M.adjust 
				(fromMaybe (RepoSize 0) . f k . Just)
				u sizemap
			liftIO $ putMVar rsv (Just sizemap')
  where
	f = case s of
		InfoPresent -> addKeyRepoSize
		InfoMissing -> removeKeyRepoSize
		InfoDead -> removeKeyRepoSize

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
	startv <- liftIO newEmptyMVar
	donev <- liftIO newEmptyMVar
	void $ liftIO $ forkIO $ waitstart startv donev h u
	return (LiveUpdate startv donev)
  where
	{- Wait for startLiveUpdate, or for the LiveUpdate to get garbage
	 - collected in the case where it is never going to start. -}
	waitstart startv donev h u = tryNonAsync (takeMVar startv) >>= \case
		Right _ -> do
			Db.startingLiveSizeChange h u k sc
			waitdone donev h u
		Left _ -> noop
	
	{- Wait for finishedLiveUpdate to be called, or for the LiveUpdate to
	 - get garbage collected in the case where the change didn't
	 - actually happen. -}
	waitdone donev h u = tryNonAsync (takeMVar donev) >>= \case
		-- TODO if succeeded == True, need to update RepoSize db
		-- in same transaction as Db.finishedLiveSizeChange
		Right (succeeded, u', k', sc')
			| u' == u && k' == k && sc' == sc -> done h u
			-- This can happen when eg, storing to a cluster
			-- causes fanout and so this is called with
			-- other UUIDs.
			| otherwise -> waitdone donev h u
		Left _ -> done h u
	done h u = Db.finishedLiveSizeChange h u k sc

-- Called when a preferred content check indicates that a live update is
-- needed. Can be called more than once.
startLiveUpdate :: LiveUpdate -> Annex ()
startLiveUpdate (LiveUpdate startv _donev) = 
	liftIO $ void $ tryPutMVar startv ()
startLiveUpdate NoLiveUpdate = noop

finishedLiveUpdate :: LiveUpdate -> Bool -> UUID -> Key -> SizeChange -> IO ()
finishedLiveUpdate (LiveUpdate _startv donev) succeeded u k sc =
	putMVar donev (succeeded, u, k, sc)
finishedLiveUpdate NoLiveUpdate _ _ _ _ = noop
