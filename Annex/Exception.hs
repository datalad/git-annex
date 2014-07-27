{- exception handling in the git-annex monad
 -
 - Note that when an Annex action fails and the exception is handled
 - by these functions, any changes the action has made to the
 - AnnexState are retained. This works because the Annex monad
 - internally stores the AnnexState in a MVar.
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Annex.Exception (
	bracketIO,
	bracketAnnex,
	tryAnnex,
	tryAnnexIO,
	throwAnnex,
	catchAnnex,
	catchNonAsyncAnnex,
	tryNonAsyncAnnex,
) where

import qualified Control.Monad.Catch as M
import Control.Exception

import Common.Annex

{- Runs an Annex action, with setup and cleanup both in the IO monad. -}
bracketIO :: IO v -> (v -> IO b) -> (v -> Annex a) -> Annex a
bracketIO setup cleanup = M.bracket (liftIO setup) (liftIO . cleanup)

bracketAnnex :: Annex v -> (v -> Annex b) -> (v -> Annex a) -> Annex a
bracketAnnex = M.bracket

{- try in the Annex monad -}
tryAnnex :: Annex a -> Annex (Either SomeException a)
tryAnnex = M.try

{- try in the Annex monad, but only catching IO exceptions -}
tryAnnexIO :: Annex a -> Annex (Either IOException a)
tryAnnexIO = M.try

{- throw in the Annex monad -}
throwAnnex :: Exception e => e -> Annex a
throwAnnex = M.throwM

{- catch in the Annex monad -}
catchAnnex :: Exception e => Annex a -> (e -> Annex a) -> Annex a
catchAnnex = M.catch

{- catchs all exceptions except for async exceptions -}
catchNonAsyncAnnex :: Annex a -> (SomeException -> Annex a) -> Annex a
catchNonAsyncAnnex a onerr = a `M.catches`
	[ M.Handler (\ (e :: AsyncException) -> throwAnnex e)
	, M.Handler (\ (e :: SomeException) -> onerr e)
	]

tryNonAsyncAnnex :: Annex a -> Annex (Either SomeException a)
tryNonAsyncAnnex a = (Right <$> a) `catchNonAsyncAnnex` (return . Left)
