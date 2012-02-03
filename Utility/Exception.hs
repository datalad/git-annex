{- Simple IO exception handling
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Exception where

import Prelude hiding (catch)
import Control.Exception
import Control.Applicative

{- Catches IO errors and returns a Bool -}
catchBoolIO :: IO Bool -> IO Bool
catchBoolIO a = catchDefaultIO a False

{- Catches IO errors and returns a Maybe -}
catchMaybeIO :: IO a -> IO (Maybe a)
catchMaybeIO a = catchDefaultIO (Just <$> a) Nothing

{- Catches IO errors and returns a default value. -}
catchDefaultIO :: IO a -> a -> IO a
catchDefaultIO a def = catchIO a (const $ return def)

{- Catches IO errors and returns the error message. -}
catchMsgIO :: IO a -> IO (Either String a)
catchMsgIO a = dispatch <$> tryIO a
	where
		dispatch (Left e) = Left $ show e
		dispatch (Right v) = Right v

{- catch specialized for IO errors only -}
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

{- try specialized for IO errors only -}
tryIO :: IO a -> IO (Either IOException a)
tryIO = try
