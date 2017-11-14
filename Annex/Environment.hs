{- git-annex environment
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Environment where

import Annex.Common
import Utility.UserInfo
import qualified Git.Config
import Config
import Utility.Env

{- Checks that the system's environment allows git to function.
 - Git requires a GECOS username, or suitable git configuration, or
 - environment variables.
 -
 - Git also requires the system have a hostname containing a dot.
 - Otherwise, it tries various methods to find a FQDN, and will fail if it
 - does not. To avoid replicating that code here, which would break if its
 - methods change, this function does not check the hostname is valid.
 - Instead, code that commits can use ensureCommit.
 -}
checkEnvironment :: Annex ()
checkEnvironment = do
	gitusername <- fromRepo $ Git.Config.getMaybe "user.name"
	when (isNothing gitusername || gitusername == Just "") $
		liftIO checkEnvironmentIO

checkEnvironmentIO :: IO ()
checkEnvironmentIO = whenM (isNothing <$> myUserGecos) $ do
	username <- either (const "unknown") id <$> myUserName
	ensureEnv "GIT_AUTHOR_NAME" username
	ensureEnv "GIT_COMMITTER_NAME" username
  where
#ifndef __ANDROID__
	-- existing environment is not overwritten
	ensureEnv var val = setEnv var val False
#else
	-- Environment setting is broken on Android, so this is dealt with
	-- in runshell instead.
	ensureEnv _ _ = noop
#endif

{- Runs an action that commits to the repository, and if it fails, 
 - sets user.email and user.name to a dummy value and tries the action again. -}
ensureCommit :: Annex a -> Annex a
ensureCommit a = either retry return =<< tryNonAsync a 
  where
	retry _ = do
		name <- liftIO $ either (const "unknown") id <$> myUserName
		setConfig (ConfigKey "user.name") name
		setConfig (ConfigKey "user.email") name
		a
