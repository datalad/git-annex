{- Simple IO exception handling (and some more)
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Utility.Exception where

import Control.Exception
import qualified Control.Exception as E
import Control.Applicative
import Control.Monad
import System.IO.Error (isDoesNotExistError)
import Utility.Data

{- Catches IO errors and returns a Bool -}
catchBoolIO :: IO Bool -> IO Bool
catchBoolIO a = catchDefaultIO False a

{- Catches IO errors and returns a Maybe -}
catchMaybeIO :: IO a -> IO (Maybe a)
catchMaybeIO a = catchDefaultIO Nothing $ Just <$> a

{- Catches IO errors and returns a default value. -}
catchDefaultIO :: a -> IO a -> IO a
catchDefaultIO def a = catchIO a (const $ return def)

{- Catches IO errors and returns the error message. -}
catchMsgIO :: IO a -> IO (Either String a)
catchMsgIO a = either (Left . show) Right <$> tryIO a

{- catch specialized for IO errors only -}
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = E.catch

{- try specialized for IO errors only -}
tryIO :: IO a -> IO (Either IOException a)
tryIO = try

{- Catches all exceptions except for async exceptions.
 - This is often better to use than catching them all, so that
 - ThreadKilled and UserInterrupt get through.
 -}
catchNonAsync :: IO a -> (SomeException -> IO a) -> IO a
catchNonAsync a onerr = a `catches`
	[ Handler (\ (e :: AsyncException) -> throw e)
	, Handler (\ (e :: SomeException) -> onerr e)
	]

tryNonAsync :: IO a -> IO (Either SomeException a)
tryNonAsync a = (Right <$> a) `catchNonAsync` (return . Left)

{- Catches only DoesNotExist exceptions, and lets all others through. -}
tryWhenExists :: IO a -> IO (Maybe a)
tryWhenExists a = eitherToMaybe <$>
	tryJust (guard . isDoesNotExistError) a
