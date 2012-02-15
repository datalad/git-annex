{- state monad support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.State where

import Control.Monad.State.Strict

{- Modifies Control.Monad.State's state, forcing a strict update.
 - This avoids building thunks in the state and leaking.
 - Why it's not the default, I don't know.
 -
 - Example: changeState $ \s -> s { foo = bar }
 -}
changeState :: MonadState s m => (s -> s) -> m ()
changeState f = do
	x <- get
	put $! f x

{- Gets a value from the internal state, selected by the passed value
 - constructor. -}
getState :: MonadState s m => (s -> a) -> m a
getState = gets
