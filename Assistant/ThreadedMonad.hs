{- making the Annex monad available across threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.ThreadedMonad where

import Common.Annex
import qualified Annex

import Control.Concurrent
import Data.Tuple

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
 - This serializes calls by threads; only one thread can run in Annex at a
 - time. -}
runThreadState :: ThreadState -> Annex a -> IO a
runThreadState mvar a = modifyMVar mvar $ \state -> swap <$> Annex.run state a

{- Runs an Annex action in a separate thread, using a copy of the state
 - from the MVar.
 -
 - It's up to the action to perform any necessary shutdown tasks in order
 - for state to not be lost. And it's up to the caller to resynchronise
 - with any changes the action makes to eg, the git-annex branch.
 -}
unsafeForkIOThreadState :: ThreadState -> Annex a -> IO ThreadId
unsafeForkIOThreadState mvar a = do
	state <- readMVar mvar
	forkIO $ void $ Annex.eval state a
