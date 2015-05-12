{- Simple IO exception handling (and some more)
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Exception (
	module X,
	catchBoolIO,
	catchMaybeIO,
	catchDefaultIO,
	catchMsgIO,
	catchIO,
	tryIO,
	bracketIO,
	catchNonAsync,
	tryNonAsync,
	tryWhenExists,
) where

import Control.Monad.Catch as X hiding (Handler)
import qualified Control.Monad.Catch as M
import Control.Exception (IOException, AsyncException)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO.Error (isDoesNotExistError)
import Utility.Data

{- Catches IO errors and returns a Bool -}
catchBoolIO :: MonadCatch m => m Bool -> m Bool
catchBoolIO = catchDefaultIO False

{- Catches IO errors and returns a Maybe -}
catchMaybeIO :: MonadCatch m => m a -> m (Maybe a)
catchMaybeIO a = do
	catchDefaultIO Nothing $ do
		v <- a
		return (Just v)

{- Catches IO errors and returns a default value. -}
catchDefaultIO :: MonadCatch m => a -> m a -> m a
catchDefaultIO def a = catchIO a (const $ return def)

{- Catches IO errors and returns the error message. -}
catchMsgIO :: MonadCatch m => m a -> m (Either String a)
catchMsgIO a = do
	v <- tryIO a
	return $ either (Left . show) Right v

{- catch specialized for IO errors only -}
catchIO :: MonadCatch m => m a -> (IOException -> m a) -> m a
catchIO = M.catch

{- try specialized for IO errors only -}
tryIO :: MonadCatch m => m a -> m (Either IOException a)
tryIO = M.try

{- bracket with setup and cleanup actions lifted to IO.
 -
 - Note that unlike catchIO and tryIO, this catches all exceptions. -}
bracketIO :: (MonadMask m, MonadIO m) => IO v -> (v -> IO b) -> (v -> m a) -> m a
bracketIO setup cleanup = bracket (liftIO setup) (liftIO . cleanup)

{- Catches all exceptions except for async exceptions.
 - This is often better to use than catching them all, so that
 - ThreadKilled and UserInterrupt get through.
 -}
catchNonAsync :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchNonAsync a onerr = a `catches`
	[ M.Handler (\ (e :: AsyncException) -> throwM e)
	, M.Handler (\ (e :: SomeException) -> onerr e)
	]

tryNonAsync :: MonadCatch m => m a -> m (Either SomeException a)
tryNonAsync a = go `catchNonAsync` (return . Left)
  where
	go = do
		v <- a
		return (Right v)

{- Catches only DoesNotExist exceptions, and lets all others through. -}
tryWhenExists :: MonadCatch m => m a -> m (Maybe a)
tryWhenExists a = do
	v <- tryJust (guard . isDoesNotExistError) a
	return (eitherToMaybe v)
