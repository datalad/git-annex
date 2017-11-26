{- git-annex branch state management
 -
 - Runtime state about the git-annex branch.
 -
 - Copyright 2011-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.BranchState where

import Annex.Common
import Types.BranchState
import qualified Annex

getState :: Annex BranchState
getState = Annex.getState Annex.branchstate

changeState :: (BranchState -> BranchState) -> Annex ()
changeState changer = Annex.changeState $ \s -> 
	s { Annex.branchstate = changer (Annex.branchstate s) }

{- Runs an action to check that the index file exists, if it's not been
 - checked before in this run of git-annex. -}
checkIndexOnce :: Annex () -> Annex ()
checkIndexOnce a = unlessM (indexChecked <$> getState) $ do
	a
	changeState $ \s -> s { indexChecked = True }

{- Runs an action to update the branch, if it's not been updated before
 - in this run of git-annex. -}
runUpdateOnce :: Annex () -> Annex ()
runUpdateOnce a = unlessM (branchUpdated <$> getState) $ do
	a
	disableUpdate

{- Avoids updating the branch. A useful optimisation when the branch
 - is known to have not changed, or git-annex won't be relying on info
 - from it. -}
disableUpdate :: Annex ()
disableUpdate = changeState $ \s -> s { branchUpdated = True }
