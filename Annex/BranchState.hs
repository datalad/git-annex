{- git-annex branch state management
 -
 - Runtime state about the git-annex branch, including a small read cache.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.BranchState where

import Common.Annex
import Types.BranchState
import qualified Annex

getState :: Annex BranchState
getState = Annex.getState Annex.branchstate

setState :: BranchState -> Annex ()
setState state = Annex.changeState $ \s -> s { Annex.branchstate = state }

changeState :: (BranchState -> BranchState) -> Annex ()
changeState changer = setState =<< changer <$> getState

setCache :: FilePath -> String -> Annex ()
setCache file content = changeState $ \s -> s
	{ cachedFile = Just file, cachedContent = content}

getCache :: FilePath -> Annex (Maybe String)
getCache file = from <$> getState
	where
		from state
			| cachedFile state == Just file =
				Just $ cachedContent state
			| otherwise = Nothing

invalidateCache :: Annex ()
invalidateCache = changeState $ \s -> s
	{ cachedFile = Nothing, cachedContent = "" }

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
