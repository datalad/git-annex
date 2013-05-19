{- exception handling in the git-annex monad
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Exception (
	bracketIO,
	handle,
	tryAnnex,
	throw,
) where

import Control.Exception.Lifted (handle, try)
import Control.Monad.Trans.Control (liftBaseOp)
import Control.Exception hiding (handle, try, throw)

import Common.Annex

{- Runs an Annex action, with setup and cleanup both in the IO monad.
 -
 - Warning: Currently if the Annex action fails, any changes it has made
 - to Annex state are discarded.
 -}
bracketIO :: IO c -> (c -> IO b) -> Annex a -> Annex a
bracketIO setup cleanup go =
	liftBaseOp (Control.Exception.bracket setup cleanup) (const go)

{- try in the Annex monad -}
tryAnnex :: Annex a -> Annex (Either SomeException a)
tryAnnex = try

{- Throws an exception in the Annex monad. -}
throw :: Control.Exception.Exception e => e -> Annex a
throw = liftIO . throwIO
