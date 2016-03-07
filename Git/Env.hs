{- Adjusting the environment while running git commands.
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Env where

import Git
import Git.Types
import Utility.Env
#ifdef __ANDROID__
import Common
#endif

{- Adjusts the gitEnv of a Repo. Copies the system environment if the repo
 - does not have any gitEnv yet. -}
adjustGitEnv :: Repo -> ([(String, String)] -> [(String, String)]) -> IO Repo
adjustGitEnv g adj = do
	e <- maybe copyenv return (gitEnv g)
	let e' = adj e
	return $ g { gitEnv = Just e' }
  where
	copyenv = do
#ifdef __ANDROID__
		{- This should not be necessary on Android, but there is some
		 - weird getEnvironment breakage. See
		 - https://github.com/neurocyte/ghc-android/issues/7
		 - Use getEnv to get some key environment variables that
		 - git expects to have. -}
		let keyenv = words "USER PATH GIT_EXEC_PATH HOSTNAME HOME"
		let getEnvPair k = maybe Nothing (\v -> Just (k, v)) <$> getEnv k
		catMaybes <$> forM keyenv getEnvPair
#else
		getEnvironment
#endif

addGitEnv :: Repo -> String -> String -> IO Repo
addGitEnv g var val = adjustGitEnv g (addEntry var val)

{- Use with any action that makes a commit to set metadata. -}
commitWithMetaData :: CommitMetaData -> CommitMetaData -> (Repo -> IO a) -> Repo -> IO a
commitWithMetaData authormetadata committermetadata a g =
	a =<< adjustGitEnv g adj
  where
	adj = mkadj "AUTHOR" authormetadata
		. mkadj "COMMITTER" committermetadata
	mkadj p md = go "NAME" commitName
		. go "EMAIL" commitEmail
		. go "DATE" commitDate
	  where
		go s getv = case getv md of
			Nothing -> id
			Just v -> addEntry ("GIT_" ++ p ++ "_" ++ s) v
