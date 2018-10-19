{- currently checked out branch
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.CurrentBranch where

import Annex.Common
import Types.AdjustedBranch
import Annex.AdjustedBranch.Name
import qualified Annex
import qualified Git
import qualified Git.Branch

type CurrBranch = (Maybe Git.Branch, Maybe Adjustment)

{- Gets the currently checked out branch.
 - When on an adjusted branch, gets the original branch, and the adjustment.
 -
 - Cached for speed.
 -
 - Until a commit is made in a new repository, no branch is checked out.
 - Since git-annex may make the first commit, this does not cache
 - the absence of a branch.
 -}
getCurrentBranch :: Annex CurrBranch
getCurrentBranch = maybe cache return
	=<< Annex.getState Annex.cachedcurrentbranch
  where
	cache = inRepo Git.Branch.current >>= \case
		Just b -> do
			let v = case adjustedToOriginal b of 
                        	Nothing -> (Just b, Nothing) 
                                Just (adj, origbranch) -> 
                                	(Just origbranch, Just adj)
			Annex.changeState $ \s ->
				s { Annex.cachedcurrentbranch = Just v }
			return v
		Nothing -> return (Nothing, Nothing)
