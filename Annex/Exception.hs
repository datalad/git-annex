{- exception handling in the git-annex monad
 -
 - Note that when an Annex action fails and the exception is handled
 - by these functions, any changes the action has made to the
 - AnnexState are retained. This works because the Annex monad
 - internally stores the AnnexState in a MVar.
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Exception (
	bracketIO,
	tryAnnex,
	throw,
	catchAnnex,
) where

import Prelude hiding (catch)
import "MonadCatchIO-transformers" Control.Monad.CatchIO (bracket, try, throw, catch)
import Control.Exception hiding (handle, try, throw, bracket, catch)

import Common.Annex

{- Runs an Annex action, with setup and cleanup both in the IO monad. -}
bracketIO :: IO c -> (c -> IO b) -> Annex a -> Annex a
bracketIO setup cleanup go =
	bracket (liftIO setup) (liftIO . cleanup) (const go)

{- try in the Annex monad -}
tryAnnex :: Annex a -> Annex (Either SomeException a)
tryAnnex = try

{- catch in the Annex monad -}
catchAnnex :: Exception e => Annex a -> (e -> Annex a) -> Annex a
catchAnnex = catch
