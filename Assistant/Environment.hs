{- git-annex assistant environment
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Environment where

import Assistant.Common
import Utility.UserInfo
import qualified Git.Config

import System.Posix.Env

{- Checks that the system's environment allows git to function.
 - Git requires a GECOS username, or suitable git configuration, or
 - environment variables. -}
checkEnvironment :: Annex ()
checkEnvironment = do
	username <- liftIO myUserName
	gecos <- liftIO myUserGecos
	gitusername <- fromRepo $ Git.Config.getMaybe "user.name"
	when (null gecos && (gitusername == Nothing || gitusername == Just "")) $
		-- existing environment is not overwritten
		liftIO $ setEnv "GIT_AUTHOR_NAME" username False
