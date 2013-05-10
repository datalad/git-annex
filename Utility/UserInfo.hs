{- user info
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.UserInfo (
	myHomeDir,
	myUserName,
	myUserGecos,
) where

import Control.Applicative
#ifndef mingw32_HOST_OS
import System.Posix.User
import System.Posix.Env
#endif

{- Current user's home directory.
 -
 - getpwent will fail on LDAP or NIS, so use HOME if set. -}
myHomeDir :: IO FilePath
#ifndef mingw32_HOST_OS
myHomeDir = myVal ["HOME"] homeDirectory
#else
myHomeDir = error "myHomeDir TODO"
#endif

{- Current user's user name. -}
myUserName :: IO String
#ifndef mingw32_HOST_OS
myUserName = myVal ["USER", "LOGNAME"] userName
#else
myUserName = error "myUserName TODO"
#endif

myUserGecos :: IO String
#ifdef __ANDROID__
myUserGecos = return "" -- userGecos crashes on Android
#else
#ifndef mingw32_HOST_OS
myUserGecos = myVal [] userGecos
#else
myUserGecos = error "myUserGecos TODO"
#endif
#endif

#ifndef mingw32_HOST_OS
myVal :: [String] -> (UserEntry -> String) -> IO String
myVal envvars extract = maybe (extract <$> getpwent) return =<< check envvars
  where
	check [] = return Nothing
	check (v:vs) = maybe (check vs) (return . Just) =<< getEnv v
	getpwent = getUserEntryForID =<< getEffectiveUserID
#endif
