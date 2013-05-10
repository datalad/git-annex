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
#if 0
import System.Posix.User
import System.Posix.Env
#endif

{- Current user's home directory.
 -
 - getpwent will fail on LDAP or NIS, so use HOME if set. -}
myHomeDir :: IO FilePath
#if 0
myHomeDir = myVal ["HOME"] homeDirectory
#else
myHomeDir = error "myHomeDir TODO"
#endif

{- Current user's user name. -}
myUserName :: IO String
#if 0
myUserName = myVal ["USER", "LOGNAME"] userName
#else
myUserName = error "myUserName TODO"
#endif

myUserGecos :: IO String
#ifdef __ANDROID__
myUserGecos = return "" -- userGecos crashes on Android
#else
#if 0
myUserGecos = myVal [] userGecos
#else
myUserGecos = error "myUserGecos TODO"
#endif
#endif

#if 0
myVal :: [String] -> (UserEntry -> String) -> IO String
myVal envvars extract = maybe (extract <$> getpwent) return =<< check envvars
  where
	check [] = return Nothing
	check (v:vs) = maybe (check vs) (return . Just) =<< getEnv v
	getpwent = getUserEntryForID =<< getEffectiveUserID
#endif
