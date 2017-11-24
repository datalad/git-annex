{- git FilePath library
 -
 - Different git commands use different types of FilePaths to refer to
 - files in the repository. Some commands use paths relative to the
 - top of the repository even when run in a subdirectory. Adding some
 - types helps keep that straight.
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.FilePath (
	TopFilePath,
	BranchFilePath(..),
	descBranchFilePath,
	getTopFilePath,
	fromTopFilePath,
	toTopFilePath,
	asTopFilePath,
	InternalGitPath,
	toInternalGitPath,
	fromInternalGitPath,
	absoluteGitPath
) where

import Common
import Git

import qualified System.FilePath.Posix

{- A FilePath, relative to the top of the git repository. -}
newtype TopFilePath = TopFilePath { getTopFilePath :: FilePath }
	deriving (Show, Eq, Ord)

{- A file in a branch or other treeish. -}
data BranchFilePath = BranchFilePath Ref TopFilePath

{- Git uses the branch:file form to refer to a BranchFilePath -}
descBranchFilePath :: BranchFilePath -> String
descBranchFilePath (BranchFilePath b f) = fromRef b ++ ':' : getTopFilePath f

{- Path to a TopFilePath, within the provided git repo. -}
fromTopFilePath :: TopFilePath -> Git.Repo -> FilePath
fromTopFilePath p repo = combine (repoPath repo) (getTopFilePath p)

{- The input FilePath can be absolute, or relative to the CWD. -}
toTopFilePath :: FilePath -> Git.Repo -> IO TopFilePath
toTopFilePath file repo = TopFilePath <$> relPathDirToFile (repoPath repo) file

{- The input FilePath must already be relative to the top of the git
 - repository -}
asTopFilePath :: FilePath -> TopFilePath
asTopFilePath file = TopFilePath file

{- Git may use a different representation of a path when storing
 - it internally. 
 -
 - On Windows, git uses '/' to separate paths stored in the repository,
 - despite Windows using '\'.
 -
 -}
type InternalGitPath = String

toInternalGitPath :: FilePath -> InternalGitPath
#ifndef mingw32_HOST_OS
toInternalGitPath = id
#else
toInternalGitPath = replace "\\" "/"
#endif

fromInternalGitPath :: InternalGitPath -> FilePath
#ifndef mingw32_HOST_OS
fromInternalGitPath = id
#else
fromInternalGitPath = replace "/" "\\"
#endif

{- isAbsolute on Windows does not think "/foo" or "\foo" is absolute,
 - so try posix paths.
 -}
absoluteGitPath :: FilePath -> Bool
absoluteGitPath p = isAbsolute p ||
	System.FilePath.Posix.isAbsolute (toInternalGitPath p)
