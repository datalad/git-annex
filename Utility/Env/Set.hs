{- portable environment variables
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Env.Set where

#ifdef mingw32_HOST_OS
import qualified System.Environment as E
import qualified System.SetEnv
import Utility.Env
#else
import qualified System.Posix.Env as PE
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
