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

import Utility.Env.Basic
import Utility.Exception
#ifndef mingw32_HOST_OS
import Utility.Data
import Control.Applicative
import System.Posix.User
#if MIN_VERSION_unix(2,8,0)
import System.Posix.User.ByteString (UserEntry)
#endif
#endif

import Prelude

{- Current user's home directory.
 -
 - getpwent will fail on LDAP or NIS, so use HOME if set. -}
myHomeDir :: IO FilePath
myHomeDir = either giveup return =<<
#ifndef mingw32_HOST_OS
	myVal ["HOME"] homeDirectory
#else
	myVal ["USERPROFILE", "HOME"] -- HOME is used in Cygwin
#endif

{- Current user's user name. -}
myUserName :: IO (Either String String)
myUserName =
#ifndef mingw32_HOST_OS
	myVal ["USER", "LOGNAME"] userName
#else
	myVal ["USERNAME", "USER", "LOGNAME"]
#endif

myUserGecos :: IO (Maybe String)
-- userGecos is not available on Windows.
#if defined(mingw32_HOST_OS)
myUserGecos = return Nothing
#else
myUserGecos = eitherToMaybe <$> myVal [] userGecos
#endif

#ifndef mingw32_HOST_OS
myVal :: [String] -> (UserEntry -> String) -> IO (Either String String)
myVal envvars extract = go envvars
  where
	go [] = either (const $ envnotset) (Right . extract) <$> get
	go (v:vs) = maybe (go vs) (return . Right) =<< getEnv v
	-- This may throw an exception if the system doesn't have a
	-- passwd file etc; don't let it crash.
	get = tryNonAsync $ getUserEntryForID =<< getEffectiveUserID
#else
myVal :: [String] -> IO (Either String String)
myVal envvars = go envvars
  where
	go [] = return envnotset
	go (v:vs) = maybe (go vs) (return . Right) =<< getEnv v
#endif
	envnotset = Left ("environment not set: " ++ show envvars)
