{- git-annex concurrent state
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Concurrent where

import Common.Annex
import Annex
import Annex.CatFile
import Annex.CheckAttr
import Annex.CheckIgnore

import qualified Data.Map as M

{- Allows forking off a thread that uses a copy of the current AnnexState
 - to run an Annex action.
 -
 - The returned IO action can be used to start the thread.
 - It returns an Annex action that must be run in the original 
 - calling context to merge the forked AnnexState back into the
 - current AnnexState.
 -}
forkState :: Annex a -> Annex (IO (Annex a))
forkState a = do
	st <- dupState
	return $ do
		(ret, newst) <- run st a
		return $ do
			mergeState newst
			return ret

{- Returns a copy of the current AnnexState that is safe to be
 - used when forking off a thread. 
 -
 - After an Annex action is run using this AnnexState, it
 - should be merged back into the current Annex's state,
 - by calling mergeState.
 -}
dupState :: Annex AnnexState
dupState = do
	st <- Annex.getState id
	-- avoid sharing eg, open file handles
	return $ st
		{ Annex.workers = []
		, Annex.catfilehandles = M.empty
		, Annex.checkattrhandle = Nothing
		, Annex.checkignorehandle = Nothing
		}

{- Merges the passed AnnexState into the current Annex state.
 - Also closes various handles in it. -}
mergeState :: AnnexState -> Annex ()
mergeState st = do
	st' <- liftIO $ snd <$> run st closehandles
	forM_ (M.toList $ Annex.cleanup st') $
		uncurry addCleanup
	changeState $ \s -> s { errcounter = errcounter s + errcounter st' }
  where
	closehandles = do
		catFileStop
		checkAttrStop
		checkIgnoreStop
