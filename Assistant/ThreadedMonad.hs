{- making the Annex monad available across threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

{-# LANGUAGE BangPatterns #-}

module Assistant.ThreadedMonad where

import Common.Annex
import qualified Annex

import Control.Concurrent

{- The Annex state is stored in a MVar, so that threaded actions can access
 - it. -}
type ThreadState = MVar Annex.AnnexState

{- Stores the Annex state in a MVar.
 -
 - Once the action is finished, retrieves the state from the MVar.
 -}
withThreadState :: (ThreadState -> Annex a) -> Annex a
withThreadState a = do
	state <- Annex.getState id
	mvar <- liftIO $ newMVar state
	r <- a mvar
	newstate <- liftIO $ takeMVar mvar
	Annex.changeState (const newstate)
	return r

{- Runs an Annex action, using the state from the MVar. 
 -
 - This serializes calls by threads. -}
runThreadState :: ThreadState -> Annex a -> IO a
runThreadState mvar a = do
	startstate <- takeMVar mvar
	!(r, newstate) <- Annex.run startstate a
	putMVar mvar newstate
	return r
