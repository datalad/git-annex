{- portable environment variables
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Env where

#ifdef mingw32_HOST_OS
import Utility.Exception
import Control.Applicative
import Data.Maybe
import qualified System.Environment as E
#else
import qualified System.Posix.Env as PE
#endif

getEnv :: String -> IO (Maybe String)
#ifndef mingw32_HOST_OS
getEnv = PE.getEnv
#else
getEnv = catchMaybeIO . E.getEnv
#endif

getEnvDefault :: String -> String -> IO String
#ifndef mingw32_HOST_OS
getEnvDefault = PE.getEnvDefault
#else
getEnvDefault var fallback = fromMaybe fallback <$> getEnv var
#endif

getEnvironment :: IO [(String, String)]
#ifndef mingw32_HOST_OS
getEnvironment = PE.getEnvironment
#else
getEnvironment = E.getEnvironment
#endif

{- Returns True if it could successfully set the environment variable.
 -
 - There is, apparently, no way to do this in Windows. Instead,
 - environment varuables must be provided when running a new process. -}
setEnv :: String -> String -> Bool -> IO Bool
#ifndef mingw32_HOST_OS
setEnv var val overwrite = do
	PE.setEnv var val overwrite
	return True
#else
setEnv _ _ _ = return False
#endif

{- Returns True if it could successfully unset the environment variable. -}
unsetEnv :: String -> IO Bool
#ifndef mingw32_HOST_OS
unsetEnv var = do
	PE.unsetEnv var
	return True
#else
unsetEnv _ = return False
#endif

{- Adds the environment variable to the input environment. If already
 - present in the list, removes the old value.
 -
 - This does not really belong here, but Data.AssocList is for some reason
 - buried inside hxt.
 -}
addEntry :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addEntry k v l = ( (k,v) : ) $! delEntry k l

addEntries :: Eq k => [(k, v)] -> [(k, v)] -> [(k, v)]
addEntries = foldr (.) id . map (uncurry addEntry) . reverse

delEntry :: Eq k => k -> [(k, v)] -> [(k, v)]
delEntry _ []   = []
delEntry k (x@(k1,_) : rest)
	| k == k1 = rest
	| otherwise = ( x : ) $! delEntry k rest
