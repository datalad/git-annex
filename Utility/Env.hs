{- portable environment variables
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Env where

#ifdef mingw32_HOST_OS
import Utility.Exception
import Control.Applicative
import Data.Maybe
import Prelude
import qualified System.Environment as E
import qualified System.SetEnv
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

{- Sets an environment variable. To overwrite an existing variable,
 - overwrite must be True.
 -
 - On Windows, setting a variable to "" unsets it. -}
setEnv :: String -> String -> Bool -> IO ()
#ifndef mingw32_HOST_OS
setEnv var val overwrite = PE.setEnv var val overwrite
#else
setEnv var val True = System.SetEnv.setEnv var val
setEnv var val False = do
	r <- getEnv var
	case r of
		Nothing -> setEnv var val True
		Just _ -> return ()
#endif

unsetEnv :: String -> IO ()
#ifndef mingw32_HOST_OS
unsetEnv = PE.unsetEnv
#else
unsetEnv = System.SetEnv.unsetEnv
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
