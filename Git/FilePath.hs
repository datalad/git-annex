{- git FilePath library
 -
 - Different git commands use different types of FilePaths to refer to
 - files in the repository. Some commands use paths relative to the
 - top of the repository even when run in a subdirectory. Adding some
 - types helps keep that straight.
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Git.Quote

import GHC.Generics
import Control.DeepSeq

{- A path relative to the top of the git repository. -}
newtype TopFilePath = TopFilePath { getTopFilePath :: OsPath }
	deriving (Show, Eq, Ord, Generic)

instance NFData TopFilePath

{- A file in a branch or other treeish. -}
data BranchFilePath = BranchFilePath Ref TopFilePath
	deriving (Show, Eq, Ord)
 
{- Git uses the branch:file form to refer to a BranchFilePath -}
descBranchFilePath :: BranchFilePath -> StringContainingQuotedPath
descBranchFilePath (BranchFilePath b f) =
	UnquotedByteString (fromRef' b) <> ":" <> QuotedPath (getTopFilePath f)

{- Path to a TopFilePath, within the provided git repo. -}
fromTopFilePath :: TopFilePath -> Git.Repo -> OsPath
fromTopFilePath p repo = combine (repoPath repo) (getTopFilePath p)

{- The input FilePath can be absolute, or relative to the CWD. -}
toTopFilePath :: OsPath -> Git.Repo -> IO TopFilePath
toTopFilePath file repo = TopFilePath <$> relPathDirToFile (repoPath repo) file

{- The input path must already be relative to the top of the git
 - repository -}
asTopFilePath :: OsPath -> TopFilePath
asTopFilePath file = TopFilePath file

{- Git may use a different representation of a path when storing
 - it internally. 
 -
 - On Windows, git uses '/' to separate paths stored in the repository,
 - despite Windows using '\'.
 -
 -}
type InternalGitPath = OsPath

toInternalGitPath :: OsPath -> InternalGitPath
#ifndef mingw32_HOST_OS
toInternalGitPath = id
#else
toInternalGitPath = toOsPath . encodeBS . replace "\\" "/" . decodeBS . fromOsPath
#endif

fromInternalGitPath :: InternalGitPath -> OsPath
#ifndef mingw32_HOST_OS
fromInternalGitPath = id
#else
fromInternalGitPath = toOsPath . encodeBS . replace "/" "\\" . decodeBS . fromOsPath
#endif

{- isAbsolute on Windows does not think "/foo" or "\foo" is absolute,
 - so try posix paths.
 -}
absoluteGitPath :: OsPath -> Bool
absoluteGitPath p = isAbsolute p || isAbsolute (toInternalGitPath p)
