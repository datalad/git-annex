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

{-# LANGUAGE PackageImports #-}

module Annex.Exception (
	bracketIO,
	tryAnnex,
	tryAnnexIO,
	throwAnnex,
	catchAnnex,
) where

import qualified "MonadCatchIO-transformers" Control.Monad.CatchIO as M
import Control.Exception

import Common.Annex

{- Runs an Annex action, with setup and cleanup both in the IO monad. -}
bracketIO :: IO v -> (v -> IO b) -> (v -> Annex a) -> Annex a
bracketIO setup cleanup = M.bracket (liftIO setup) (liftIO . cleanup)

{- try in the Annex monad -}
tryAnnex :: Annex a -> Annex (Either SomeException a)
tryAnnex = M.try

{- try in the Annex monad, but only catching IO exceptions -}
tryAnnexIO :: Annex a -> Annex (Either IOException a)
tryAnnexIO = M.try

{- throw in the Annex monad -}
throwAnnex :: Exception e => e -> Annex a
throwAnnex = M.throw

{- catch in the Annex monad -}
catchAnnex :: Exception e => Annex a -> (e -> Annex a) -> Annex a
catchAnnex = M.catch
