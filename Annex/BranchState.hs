{- git-annex branch state management
 -
 - Runtime state about the git-annex branch, and a small cache.
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.BranchState where

import Annex.Common
import Types.BranchState
import Types.Transitions
import qualified Annex
import Logs
import qualified Git

import Control.Concurrent
import qualified Data.ByteString.Lazy as L

getState :: Annex BranchState
getState = do
	v <- Annex.getRead Annex.branchstate
	liftIO $ readMVar v

changeState :: (BranchState -> BranchState) -> Annex ()
changeState changer = do
	v <- Annex.getRead Annex.branchstate
	liftIO $ modifyMVar_ v $ return . changer

{- Runs an action to check that the index file exists, if it's not been
 - checked before in this run of git-annex. -}
checkIndexOnce :: Annex () -> Annex ()
checkIndexOnce a = unlessM (indexChecked <$> getState) $ do
	a
	changeState $ \s -> s { indexChecked = True }

data UpdateMade 
	= UpdateMade
		{ refsWereMerged :: Bool
		, journalClean :: Bool
		}
	| UpdateFailedPermissions
		{ refsUnmerged :: [Git.Sha]
		, newTransitions :: [TransitionCalculator]
		}

{- Runs an action to update the branch, if it's not been updated before
 - in this run of git-annex. 
 -
 - When interactive access is enabled, the journal is always checked when
 - reading values from the branch, and so this does not need to update
 - the branch.
 -
 - When the action leaves the journal clean, by staging anything that
 - was in it, an optimisation is enabled: The journal does not need to
 - be checked going forward, until new information gets written to it.
 -
 - When the action is unable to update the branch due to a permissions
 - problem, the journal is still read every time.
 -}
runUpdateOnce :: Annex UpdateMade -> Annex BranchState
runUpdateOnce update = do
	st <- getState
	if branchUpdated st || needInteractiveAccess st
		then return st
		else do
			um <- update
			let stf = case um of
				UpdateMade {} -> \st' -> st'
					{ branchUpdated = True
					, journalIgnorable = journalClean um
					}
				UpdateFailedPermissions {} -> \st' -> st'
					{ branchUpdated = True
					, journalIgnorable = False
					, unmergedRefs = refsUnmerged um
					, unhandledTransitions = newTransitions um
					, cachedFileContents = []
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
	-- This assumes that another thread is not setting journalIgnorable
	-- at the same time, but since runUpdateOnce is the only
	-- thing that sets it, and it only runs once, that
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
enableInteractiveBranchAccess = changeState $ \s -> s 
	{ needInteractiveAccess = True
	, journalIgnorable = False
	}

setCache :: RawFilePath -> L.ByteString -> Annex ()
setCache file content = changeState $ \s -> s
	{ cachedFileContents = add (cachedFileContents s) }
  where
	add l
		| length l < logFilesToCache = (file, content) : l
		| otherwise = (file, content) : Prelude.init l

getCache :: RawFilePath -> BranchState -> Maybe L.ByteString
getCache file state = go (cachedFileContents state)
  where
	go [] = Nothing
	go ((f,c):rest)
		| f == file && not (needInteractiveAccess state) = Just c
		| otherwise = go rest

invalidateCache :: Annex ()
invalidateCache = changeState $ \s -> s { cachedFileContents = [] }
