{- git index file stuff
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Index where

import Common
import Git
import Git.FilePath
import Utility.Env

indexEnv :: String
indexEnv = "GIT_INDEX_FILE"

{- When relative, GIT_INDEX_FILE is interpreted by git as being 
 - relative to the top of the work tree of the git repository,
 - not to the CWD. -}
indexEnvVal :: FilePath -> Repo -> IO String
indexEnvVal index r
	| isAbsolute index = return index
	| otherwise = getTopFilePath <$> toTopFilePath index r

{- Forces git to use the specified index file.
 -
 - Returns an action that will reset back to the default
 - index file.
 -
 - Warning: Not thread safe.
 -}
override :: FilePath -> Repo -> IO (IO ())
override index r = do
	res <- getEnv var
	val <- indexEnvVal index r
	setEnv var val True
	return $ reset res
  where
	var = "GIT_INDEX_FILE"
	reset (Just v) = setEnv indexEnv v True
	reset _ = unsetEnv var

indexFile :: Repo -> FilePath
indexFile r = localGitDir r </> "index"

{- Git locks the index by creating this file. -}
indexFileLock :: Repo -> FilePath
indexFileLock r = indexFile r ++ ".lock"

{- When the pre-commit hook is run, and git commit has been run with
 - a file or files specified to commit, rather than committing the staged
 - index, git provides the pre-commit hook with a "false index file".
 -
 - Changes made to this index will influence the commit, but won't
 - affect the real index file.
 -
 - This detects when we're in this situation, using a heuristic, which
 - might be broken by changes to git. Any use of this should have a test
 - case to make sure it works.
 -}
haveFalseIndex :: IO Bool
haveFalseIndex = maybe (False) check <$> getEnv indexEnv
  where
	check f = "next-index" `isPrefixOf` takeFileName f
