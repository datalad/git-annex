{- git-annex environment
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Environment where

import Common.Annex
import Utility.UserInfo
import qualified Git.Config

import System.Posix.Env

{- Checks that the system's environment allows git to function.
 - Git requires a GECOS username, or suitable git configuration, or
 - environment variables. -}
checkEnvironment :: Annex ()
checkEnvironment = do
	gitusername <- fromRepo $ Git.Config.getMaybe "user.name"
	when (gitusername == Nothing || gitusername == Just "") $
		liftIO checkEnvironmentIO

checkEnvironmentIO :: IO ()
checkEnvironmentIO = do
	whenM (null <$> myUserGecos) $ do
		username <- myUserName
		-- existing environment is not overwritten
		setEnv "GIT_AUTHOR_NAME" username False
		setEnv "GIT_COMMITTER_NAME" username False
