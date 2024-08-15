{- git-annex repo sizes
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.RepoSize (
	getRepoSizes,
) where

import Annex.Common
import qualified Annex
import Annex.Branch (UnmergedBranches(..), getBranch)
import Types.RepoSize
import qualified Database.RepoSize as Db
import Logs.Location
import Logs.UUID
import Git.Types (Sha)

import qualified Data.Map.Strict as M

{- Gets the repo size map. Cached for speed. -}
getRepoSizes :: Annex (M.Map UUID RepoSize)
getRepoSizes = maybe updateRepoSizes return =<< Annex.getState Annex.reposizes

{- Updates Annex.reposizes with current information from the git-annex
 - branch, supplimented with journalled but not yet committed information.
 -}
updateRepoSizes :: Annex (M.Map UUID RepoSize)
updateRepoSizes = bracket Db.openDb Db.closeDb $ \h -> do
	(oldsizemap, moldbranchsha) <- liftIO $ Db.getRepoSizes h
	case moldbranchsha of
		Nothing -> calculatefromscratch h >>= set
		Just oldbranchsha -> do
			currbranchsha <- getBranch
			if oldbranchsha == currbranchsha
				then journalledRepoSizes oldsizemap oldbranchsha
					>>= set
				else do
					-- XXX todo incremental update by diffing
					-- from old to new branch.
					calculatefromscratch h >>= set
  where
	calculatefromscratch h = do
		showSideAction "calculating repository sizes"
		(sizemap, branchsha) <- calcBranchRepoSizes
		liftIO $ Db.setRepoSizes h sizemap branchsha
		journalledRepoSizes sizemap branchsha
	set sizemap = do
		Annex.changeState $ \st -> st
			{ Annex.reposizes = Just sizemap }
		return sizemap

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
journalledRepoSizes :: M.Map UUID RepoSize -> Sha -> Annex (M.Map UUID RepoSize)
journalledRepoSizes startmap branchsha =
	overLocationLogsJournal startmap branchsha accumsizes
  where
	accumsizes k (newlocs, removedlocs) m = return $
		let m' = foldl' (flip $ M.alter $ addKeyRepoSize k) m newlocs
		in foldl' (flip $ M.alter $ removeKeyRepoSize k) m' removedlocs

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
