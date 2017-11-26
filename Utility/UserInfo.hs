{- user info
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.UserInfo (
	myHomeDir,
	myUserName,
	myUserGecos,
) where

import Utility.Env
import Utility.Exception
#ifndef mingw32_HOST_OS
import Utility.Data
import Control.Applicative
#endif

import System.PosixCompat
import Prelude

{- Current user's home directory.
 -
 - getpwent will fail on LDAP or NIS, so use HOME if set. -}
myHomeDir :: IO FilePath
myHomeDir = either giveup return =<< myVal env homeDirectory
  where
#ifndef mingw32_HOST_OS
	env = ["HOME"]
#else
	env = ["USERPROFILE", "HOME"] -- HOME is used in Cygwin
#endif

{- Current user's user name. -}
myUserName :: IO (Either String String)
myUserName = myVal env userName
  where
#ifndef mingw32_HOST_OS
	env = ["USER", "LOGNAME"]
#else
	env = ["USERNAME", "USER", "LOGNAME"]
#endif

myUserGecos :: IO (Maybe String)
-- userGecos crashes on Android and is not available on Windows.
#if defined(__ANDROID__) || defined(mingw32_HOST_OS)
myUserGecos = return Nothing
#else
myUserGecos = eitherToMaybe <$> myVal [] userGecos
#endif

myVal :: [String] -> (UserEntry -> String) -> IO (Either String String)
myVal envvars extract = go envvars
  where
#ifndef mingw32_HOST_OS
	go [] = Right . extract <$> (getUserEntryForID =<< getEffectiveUserID)
#else
	go [] = return $ either Left (Right . extract) $
		Left ("environment not set: " ++ show envvars)
#endif
	go (v:vs) = maybe (go vs) (return . Right) =<< getEnv v
