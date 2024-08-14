{- git-annex repo sizes
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.RepoSize where

import Annex.Common
import Annex.Branch (UnmergedBranches(..))
import Types.RepoSize
import Logs.Location
import Logs.UUID
import Git.Types (Sha)

import qualified Data.Map.Strict as M

{- Sum up the sizes of all keys in all repositories, from the information
 - in the git-annex branch, but not the journal. Retuns the sha of the
 - branch commit that was used.
 -
 - The map includes the UUIDs of all known repositories, including
 - repositories that are empty.
 -}
calcBranchRepoSizes :: Annex (M.Map UUID RepoSize, Sha)
calcBranchRepoSizes = do
	knownuuids <- M.keys <$> uuidDescMap
	let startmap = M.fromList $ map (\u -> (u, RepoSize 0)) knownuuids
	overLocationLogs True startmap accum >>= \case
		UnmergedBranches v -> return v
		NoUnmergedBranches v -> return v
  where
	addksz ksz (Just (RepoSize sz)) = Just $ RepoSize $ sz + ksz
	addksz ksz Nothing = Just $ RepoSize ksz
	accum k locs m = return $ 
		let sz = fromMaybe 0 $ fromKey keySize k
		in foldl' (flip $ M.alter $ addksz sz) m locs

{- Given the RepoSizes calculated from the git-annex branch, updates it with
 - data from journalled location logs.
  -}
journalledRepoSizes :: M.Map UUID RepoSize -> Sha -> Annex (M.Map UUID RepoSize)
journalledRepoSizes m branchsha = undefined --- XXX 
