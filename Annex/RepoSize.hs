{- git-annex repo sizes
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Annex.RepoSize (
	getRepoSizes,
) where

import Annex.Common
import Annex.RepoSize.LiveUpdate
import qualified Annex
import Annex.Branch (UnmergedBranches(..), getBranch)
import Types.RepoSize
import qualified Database.RepoSize as Db
import Logs.Location
import Logs.UUID
import Git.Types (Sha)

import Control.Concurrent
import qualified Data.Map.Strict as M

{- Gets the repo size map. Cached for speed. -}
getRepoSizes :: Annex (M.Map UUID RepoSize)
getRepoSizes = do
	rsv <- Annex.getRead Annex.reposizes
	liftIO (takeMVar rsv) >>= \case
		Just sizemap -> do
			liftIO $ putMVar rsv (Just sizemap)
			return sizemap
		Nothing -> calcRepoSizes rsv

{- Fills an empty Annex.reposizes MVar with current information
 - from the git-annex branch, supplimented with journalled but
 - not yet committed information.
 -}
calcRepoSizes :: MVar (Maybe (M.Map UUID RepoSize)) -> Annex (M.Map UUID RepoSize)
calcRepoSizes rsv = bracket setup cleanup $ \h -> go h `onException` failed
  where
	go h = do
		(oldsizemap, moldbranchsha) <- liftIO $ Db.getRepoSizes h
		!sizemap <- case moldbranchsha of
			Nothing -> calculatefromscratch h
			Just oldbranchsha -> do
				currbranchsha <- getBranch
				if oldbranchsha == currbranchsha
					then calcJournalledRepoSizes oldsizemap oldbranchsha
					else do
						-- XXX todo incremental update by diffing
						-- from old to new branch.
						calculatefromscratch h
		liftIO $ putMVar rsv (Just sizemap)
		return sizemap
	
	calculatefromscratch h = do
		showSideAction "calculating repository sizes"
		(sizemap, branchsha) <- calcBranchRepoSizes
		liftIO $ Db.setRepoSizes h sizemap branchsha
		calcJournalledRepoSizes sizemap branchsha
	
	setup = Db.openDb

	cleanup = Db.closeDb

	failed = do
		liftIO $ putMVar rsv (Just M.empty)
		return M.empty

{- Sum up the sizes of all keys in all repositories, from the information
 - in the git-annex branch, but not the journal. Retuns the sha of the
 - branch commit that was used.
 -
 - The map includes the UUIDs of all known repositories, including
 - repositories that are empty.
 -
 - Note that private repositories, which do not get recorded in
 - the git-annex branch, will have 0 size. journalledRepoSizes
 - takes care of getting repo sizes for those.
 -}
calcBranchRepoSizes :: Annex (M.Map UUID RepoSize, Sha)
calcBranchRepoSizes = do
	knownuuids <- M.keys <$> uuidDescMap
	let startmap = M.fromList $ map (\u -> (u, RepoSize 0)) knownuuids
	overLocationLogs True startmap accumsizes >>= \case
		UnmergedBranches v -> return v
		NoUnmergedBranches v -> return v
  where
	accumsizes k locs m = return $
		foldl' (flip $ M.alter $ addKeyRepoSize k) m locs

{- Given the RepoSizes calculated from the git-annex branch, updates it with
 - data from journalled location logs.
  -}
calcJournalledRepoSizes
	:: M.Map UUID RepoSize
	-> Sha 
	-> Annex (M.Map UUID RepoSize)
calcJournalledRepoSizes startmap branchsha =
	overLocationLogsJournal startmap branchsha accumsizes
  where
	accumsizes k (newlocs, removedlocs) m = return $
		let m' = foldl' (flip $ M.alter $ addKeyRepoSize k) m newlocs
		in foldl' (flip $ M.alter $ removeKeyRepoSize k) m' removedlocs
