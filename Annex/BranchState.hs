{- git-annex branch state management
 -
 - Runtime state about the git-annex branch.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
runUpdateOnce :: Annex () -> Annex BranchState
runUpdateOnce a = do
	st <- getState
	if branchUpdated st
		then return st
		else do
			a
			let stf = \st' -> st'
				{ branchUpdated = True
				-- The update staged anything that was
				-- journalled before, so the journal
				-- does not need to be checked going
				-- forward, unless new information
				-- gets written to it, or unless
				-- this run of git-annex needs to notice
				-- changes journalled by other processes
				-- while it's running.
				, journalIgnorable = not $
					journalNeverIgnorable st'
				}
			changeState stf
			return (stf st)

{- Avoids updating the branch. A useful optimisation when the branch
 - is known to have not changed, or git-annex won't be relying on info
 - from it. -}
disableUpdate :: Annex ()
disableUpdate = changeState $ \s -> s { branchUpdated = True }

{- Called when a change is made to the journal. -}
journalChanged :: Annex ()
journalChanged = do
	-- Optimisation: Typically journalIgnorable will already be True
	-- (when one thing gets journalled, often other things do to),
	-- so avoid an unnecessary write to the MVar that changeState
	-- would do.
	--
	-- This assumes that another thread is not changing journalIgnorable
	-- at the same time, but since runUpdateOnce is the only
	-- thing that changes it, and it only runs once, that
	-- should not happen.
	st <- getState 
	when (journalIgnorable st) $
		changeState $ \st' -> st' { journalIgnorable = False }

{- When git-annex is somehow interactive, eg in --batch mode,
 - and needs to always notice changes made to the journal by other
 - processes, this disables optimisations that avoid normally reading the
 - journal.
 -}
enableInteractiveJournalAccess :: Annex ()
enableInteractiveJournalAccess = changeState $
	\s -> s { journalNeverIgnorable = True }
