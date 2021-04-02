{- git-annex branch state management
 -
 - Runtime state about the git-annex branch, and a small cache.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.BranchState where

import Annex.Common
import Types.BranchState
import qualified Annex
import Logs

import qualified Data.ByteString.Lazy as L

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
 - in this run of git-annex. 
 -
 - The action should return True if anything that was in the journal
 - before got staged (or if the journal was empty). That lets an opmisation
 - be done: The journal then does not need to be checked going forward,
 - until new information gets written to it.
 -}
runUpdateOnce :: Annex Bool -> Annex BranchState
runUpdateOnce a = do
	st <- getState
	if branchUpdated st
		then return st
		else do
			journalstaged <- a
			let stf = \st' -> st'
				{ branchUpdated = True
				, journalIgnorable = journalstaged 
					&& not (needInteractiveAccess st')
				}
			changeState stf
			return (stf st)

{- Avoids updating the branch. A useful optimisation when the branch
 - is known to have not changed, or git-annex won't be relying on info
 - queried from it being as up-to-date as possible. -}
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
 -
 - It also avoids using the cache, so changes committed by other processes
 - will be seen.
 -}
enableInteractiveBranchAccess :: Annex ()
enableInteractiveBranchAccess = changeState $
	\s -> s { needInteractiveAccess = True }

setCache :: RawFilePath -> L.ByteString -> Annex ()
setCache file content = changeState $ \s -> s
	{ cachedFileContents = add (cachedFileContents s) }
  where
	add l
		| length l < logFilesToCache = (file, content) : l
		| otherwise = (file, content) : Prelude.init l

getCache :: RawFilePath -> Annex (Maybe L.ByteString)
getCache file = (\st -> go (cachedFileContents st) st) <$> getState
  where
	go [] _ = Nothing
	go ((f,c):rest) state
		| f == file && not (needInteractiveAccess state) = Just c
		| otherwise = go rest state

invalidateCache :: Annex ()
invalidateCache = changeState $ \s -> s { cachedFileContents = [] }
