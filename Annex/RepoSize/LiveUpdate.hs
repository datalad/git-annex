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
import Types.RepoSize
import Logs.Presence.Pure

import Control.Concurrent
import qualified Data.Map.Strict as M
import qualified Data.Set as S

updateRepoSize :: UUID -> Key -> LogStatus -> Annex ()
updateRepoSize u k s = do
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
