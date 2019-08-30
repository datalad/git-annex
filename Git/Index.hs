{- git index file stuff
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Index where

import Common
import Git
import Utility.Env
import Utility.Env.Set

indexEnv :: String
indexEnv = "GIT_INDEX_FILE"

{- Gets value to set GIT_INDEX_FILE to. Input should be absolute path,
 - or relative to the CWD.
 -
 - When relative, GIT_INDEX_FILE is interpreted by git as being 
 - relative to the top of the work tree of the git repository,
 - not to the CWD. Worse, other environment variables (GIT_WORK_TREE)
 - or git options (--work-tree) or configuration (core.worktree)
 - can change what the relative path is interpreted relative to.
 -
 - So, an absolute path is the only safe option for this to return.
 -}
indexEnvVal :: FilePath -> IO String
indexEnvVal = absPath

{- Forces git to use the specified index file.
 -
 - Returns an action that will reset back to the default
 - index file.
 -
 - Warning: Not thread safe.
 -}
override :: FilePath -> Repo -> IO (IO ())
override index _r = do
	res <- getEnv var
	val <- indexEnvVal index
	setEnv var val True
	return $ reset res
  where
	var = "GIT_INDEX_FILE"
	reset (Just v) = setEnv indexEnv v True
	reset _ = unsetEnv var

{- The normal index file. Does not check GIT_INDEX_FILE. -}
indexFile :: Repo -> FilePath
indexFile r = localGitDir r </> "index"

{- The index file git will currently use, checking GIT_INDEX_FILE. -}
currentIndexFile :: Repo -> IO FilePath
currentIndexFile r = fromMaybe (indexFile r) <$> getEnv indexEnv	

{- Git locks the index by creating this file. -}
indexFileLock :: FilePath -> FilePath
indexFileLock f = f ++ ".lock"
