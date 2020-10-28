{- git FilePath library
 -
 - Different git commands use different types of FilePaths to refer to
 - files in the repository. Some commands use paths relative to the
 - top of the repository even when run in a subdirectory. Adding some
 - types helps keep that straight.
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
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
import Utility.Path.AbsRel

import qualified System.FilePath.ByteString as P
import qualified System.FilePath.Posix.ByteString
import GHC.Generics
import Control.DeepSeq
import qualified Data.ByteString as S

{- A RawFilePath, relative to the top of the git repository. -}
newtype TopFilePath = TopFilePath { getTopFilePath :: RawFilePath }
	deriving (Show, Eq, Ord, Generic)

instance NFData TopFilePath

{- A file in a branch or other treeish. -}
data BranchFilePath = BranchFilePath Ref TopFilePath
	deriving (Show, Eq, Ord)

{- Git uses the branch:file form to refer to a BranchFilePath -}
descBranchFilePath :: BranchFilePath -> S.ByteString
descBranchFilePath (BranchFilePath b f) =
	fromRef' b <> ":" <> getTopFilePath f

{- Path to a TopFilePath, within the provided git repo. -}
fromTopFilePath :: TopFilePath -> Git.Repo -> RawFilePath
fromTopFilePath p repo = P.combine (repoPath repo) (getTopFilePath p)

{- The input FilePath can be absolute, or relative to the CWD. -}
toTopFilePath :: RawFilePath -> Git.Repo -> IO TopFilePath
toTopFilePath file repo = TopFilePath <$> relPathDirToFile (repoPath repo) file

{- The input RawFilePath must already be relative to the top of the git
 - repository -}
asTopFilePath :: RawFilePath -> TopFilePath
asTopFilePath file = TopFilePath file

{- Git may use a different representation of a path when storing
 - it internally. 
 -
 - On Windows, git uses '/' to separate paths stored in the repository,
 - despite Windows using '\'.
 -
 -}
type InternalGitPath = RawFilePath

toInternalGitPath :: RawFilePath -> InternalGitPath
#ifndef mingw32_HOST_OS
toInternalGitPath = id
#else
toInternalGitPath = encodeBS . replace "\\" "/" . decodeBS
#endif

fromInternalGitPath :: InternalGitPath -> RawFilePath
#ifndef mingw32_HOST_OS
fromInternalGitPath = id
#else
fromInternalGitPath = encodeBS . replace "/" "\\" . decodeBS
#endif

{- isAbsolute on Windows does not think "/foo" or "\foo" is absolute,
 - so try posix paths.
 -}
absoluteGitPath :: RawFilePath -> Bool
absoluteGitPath p = P.isAbsolute p ||
	System.FilePath.Posix.ByteString.isAbsolute (toInternalGitPath p)
