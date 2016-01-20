{- making the Annex monad available across threads
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.ThreadedMonad where

import Annex.Common
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
