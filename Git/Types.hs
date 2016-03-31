{- git data types
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Types where

import Network.URI
import qualified Data.Map as M
import System.Posix.Types
import Utility.SafeCommand

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
	, config :: M.Map String String
	-- a given git config key can actually have multiple values
	, fullconfig :: M.Map String [String]
	, remotes :: [Repo]
	-- remoteName holds the name used for this repo in remotes
	, remoteName :: Maybe RemoteName
	-- alternate environment to use when running git commands
	, gitEnv :: Maybe [(String, String)]
	-- global options to pass to git when running git commands
	, gitGlobalOpts :: [CommandParam]
	} deriving (Show, Eq, Ord)

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
	deriving (Eq)

instance Show ObjectType where
	show BlobObject = "blob"
	show CommitObject = "commit"
	show TreeObject = "tree"

readObjectType :: String -> Maybe ObjectType
readObjectType "blob" = Just BlobObject
readObjectType "commit" = Just CommitObject
readObjectType "tree" = Just TreeObject
readObjectType _ = Nothing

{- Types of blobs. -}
data BlobType = FileBlob | ExecutableBlob | SymlinkBlob
	deriving (Eq)

{- Git uses magic numbers to denote the type of a blob. -}
instance Show BlobType where
	show FileBlob = "100644"
	show ExecutableBlob = "100755"
	show SymlinkBlob = "120000"

readBlobType :: String -> Maybe BlobType
readBlobType "100644" = Just FileBlob
readBlobType "100755" = Just ExecutableBlob
readBlobType "120000" = Just SymlinkBlob
readBlobType _ = Nothing

toBlobType :: FileMode -> Maybe BlobType
toBlobType 0o100644 = Just FileBlob
toBlobType 0o100755 = Just ExecutableBlob
toBlobType 0o120000 = Just SymlinkBlob
toBlobType _ = Nothing

fromBlobType :: BlobType -> FileMode
fromBlobType FileBlob = 0o100644
fromBlobType ExecutableBlob = 0o100755
fromBlobType SymlinkBlob = 0o120000

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
