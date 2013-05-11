{- portable environment variables
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Env where

#ifdef __WINDOWS__
import qualified System.Environment as E
import Utility.Exception
#else
import qualified System.Posix.Environment as E
#endif

{- Posix getEnv is faster than the one in System.Environment,
 - so use when available. -}
getEnv :: String -> IO (Maybe String)
#ifndef __WINDOWS__
getEnv = E.getEnv
#else
getEnv = catchMaybeIO . E.getEnv
#endif

{- Returns True if it could successfully set the environment variable.
 -
 - There is, apparently, no way to do this in Windows. Instead,
 - environment varuables must be provided when running a new process. -}
setEnv :: String -> String -> IO Bool
#ifndef __WINDOWS__
setEnv var val = do
	E.setEnv var val
	return True
#else
setEnv _ _ = return False
#endif
