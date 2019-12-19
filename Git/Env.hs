{- Adjusting the environment while running git commands.
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Env where

import Common
import Git
import Git.Types
import Utility.Env

{- Adjusts the gitEnv of a Repo. Copies the system environment if the repo
 - does not have any gitEnv yet. -}
adjustGitEnv :: Repo -> ([(String, String)] -> [(String, String)]) -> IO Repo
adjustGitEnv g adj = do
	e <- maybe getEnvironment return (gitEnv g)
	let e' = adj e
	return $ g { gitEnv = Just e' }
  where

addGitEnv :: Repo -> String -> String -> IO Repo
addGitEnv g var val = adjustGitEnv g (addEntry var val)

{- Environment variables to use when running a command.
 - Includes GIT_DIR pointing at the repo, and GIT_WORK_TREE when the repo
 - is not bare. Also includes anything added to the Repo's gitEnv,
 - and a copy of the rest of the system environment. -}
propGitEnv :: Repo -> IO [(String, String)]
propGitEnv g = do
	g' <- addGitEnv g "GIT_DIR" (fromRawFilePath (localGitDir g))
	g'' <- maybe (pure g')
		(addGitEnv g' "GIT_WORK_TREE" . fromRawFilePath)
		(repoWorkTree g)
	return $ fromMaybe [] (gitEnv g'')

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
