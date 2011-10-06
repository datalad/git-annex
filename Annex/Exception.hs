{- exception handling in the git-annex monad
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Exception (
	bracketIO,
	handle,
	throw,
) where

import Control.Exception.Control (handle)
import Control.Monad.IO.Control (liftIOOp)
import Control.Exception hiding (handle, throw)

import Common.Annex

{- Runs an Annex action, with setup and cleanup both in the IO monad. -}
bracketIO :: IO c -> (c -> IO b) -> Annex a -> Annex a
bracketIO setup cleanup go =
	liftIOOp (Control.Exception.bracket setup cleanup) (const go)

{- Throws an exception in the Annex monad. -}
throw :: Control.Exception.Exception e => e -> Annex a
throw = liftIO . throwIO
