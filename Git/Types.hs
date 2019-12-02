{- git data types
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Types where

import Network.URI
import Data.String
import qualified Data.Map as M
import qualified Data.ByteString as S
import System.Posix.Types
import Utility.SafeCommand
import Utility.FileSystemEncoding

{- Support repositories on local disk, and repositories accessed via an URL.
 -
 - Repos on local disk have a git directory, and unless bare, a worktree.
 -
 - A local repo may not have had its config read yet, in which case all
 - that's known about it is its path.
 -
 - Finally, an Unknown repository may be known to exist, but nothing
 - else known about it.
 -}
data RepoLocation
	= Local { gitdir :: FilePath, worktree :: Maybe FilePath }
	| LocalUnknown FilePath
	| Url URI
	| Unknown
	deriving (Show, Eq, Ord)

data Repo = Repo
	{ location :: RepoLocation
	, config :: M.Map ConfigKey S.ByteString
	-- a given git config key can actually have multiple values
	, fullconfig :: M.Map ConfigKey [S.ByteString]
	-- remoteName holds the name used for this repo in some other
	-- repo's list of remotes, when this repo is such a remote
	, remoteName :: Maybe RemoteName
	-- alternate environment to use when running git commands
	, gitEnv :: Maybe [(String, String)]
	, gitEnvOverridesGitDir :: Bool
	-- global options to pass to git when running git commands
	, gitGlobalOpts :: [CommandParam]
	} deriving (Show, Eq, Ord)

newtype ConfigKey = ConfigKey S.ByteString
	deriving (Ord, Eq)

fromConfigKey :: ConfigKey -> String
fromConfigKey (ConfigKey s) = decodeBS' s

instance Show ConfigKey where
	show = fromConfigKey

instance IsString ConfigKey where
	fromString = ConfigKey . encodeBS'

type RemoteName = String

{- A git ref. Can be a sha1, or a branch or tag name. -}
newtype Ref = Ref String
	deriving (Eq, Ord, Read, Show)

fromRef :: Ref -> String
fromRef (Ref s) = s

{- Aliases for Ref. -}
type Branch = Ref
type Sha = Ref
type Tag = Ref

{- A date in the format described in gitrevisions. Includes the
 - braces, eg, "{yesterday}" -}
newtype RefDate = RefDate String

{- Types of objects that can be stored in git. -}
data ObjectType = BlobObject | CommitObject | TreeObject

readObjectType :: S.ByteString -> Maybe ObjectType
readObjectType "blob" = Just BlobObject
readObjectType "commit" = Just CommitObject
readObjectType "tree" = Just TreeObject
readObjectType _ = Nothing

fmtObjectType :: ObjectType -> S.ByteString
fmtObjectType BlobObject = "blob"
fmtObjectType CommitObject = "commit"
fmtObjectType TreeObject = "tree"

{- Types of items in a tree. -}
data TreeItemType = TreeFile | TreeExecutable | TreeSymlink | TreeSubmodule
	deriving (Eq)

{- Git uses magic numbers to denote the type of a tree item. -}
readTreeItemType :: S.ByteString -> Maybe TreeItemType
readTreeItemType "100644" = Just TreeFile
readTreeItemType "100755" = Just TreeExecutable
readTreeItemType "120000" = Just TreeSymlink
readTreeItemType "160000" = Just TreeSubmodule
readTreeItemType _ = Nothing

fmtTreeItemType :: TreeItemType -> S.ByteString
fmtTreeItemType TreeFile = "100644"
fmtTreeItemType TreeExecutable = "100755"
fmtTreeItemType TreeSymlink = "120000"
fmtTreeItemType TreeSubmodule = "160000"

toTreeItemType :: FileMode -> Maybe TreeItemType
toTreeItemType 0o100644 = Just TreeFile
toTreeItemType 0o100755 = Just TreeExecutable
toTreeItemType 0o120000 = Just TreeSymlink
toTreeItemType 0o160000 = Just TreeSubmodule
toTreeItemType _ = Nothing

fromTreeItemType :: TreeItemType -> FileMode
fromTreeItemType TreeFile = 0o100644
fromTreeItemType TreeExecutable = 0o100755
fromTreeItemType TreeSymlink = 0o120000
fromTreeItemType TreeSubmodule = 0o160000

data Commit = Commit
	{ commitTree :: Sha
	, commitParent :: [Sha]
	, commitAuthorMetaData :: CommitMetaData
	, commitCommitterMetaData :: CommitMetaData
	, commitMessage :: String
	}
	deriving (Show)

data CommitMetaData = CommitMetaData
	{ commitName :: Maybe String
	, commitEmail :: Maybe String
	, commitDate :: Maybe String -- In raw git form, "epoch -tzoffset"
	}
	deriving (Show)
