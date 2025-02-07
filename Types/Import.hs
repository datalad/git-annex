{- git-annex import types
 -
 - Copyright 2019-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
{-# LANGUAGE CPP #-}

module Types.Import where

import qualified Data.ByteString as S
import Data.Char
import Control.DeepSeq
import GHC.Generics
#ifdef WITH_OSPATH
import qualified System.OsPath.Posix as Posix
import System.OsString.Internal.Types
#else
import qualified System.FilePath.Posix.ByteString as Posix
#endif

import Types.Export
import Utility.QuickCheck
import Utility.FileSystemEncoding
import Utility.OsPath

{- Location of content on a remote that can be imported. 
 - This is just an alias to ExportLocation, because both are referring to a
 - location on the remote. -}
type ImportLocation = ExportLocation

mkImportLocation :: OsPath -> ImportLocation
mkImportLocation = mkExportLocation

fromImportLocation :: ImportLocation -> OsPath
fromImportLocation = fromExportLocation

{- An identifier for content stored on a remote that has been imported into
 - the repository. It should be reasonably short since it is stored in the
 - git-annex branch.
 -
 - Since other things than git-annex can modify files on import remotes,
 - and git-annex then be used to import those modifications, the
 - ContentIdentifier needs to change when a file gets changed in such a
 - way. Device, inode, and size is one example of a good content
 - identifier. Or a hash if the remote's interface exposes hashes.
 -}
newtype ContentIdentifier = ContentIdentifier S.ByteString
	deriving (Eq, Ord, Show, Generic)

instance NFData ContentIdentifier

instance Arbitrary ContentIdentifier where
	-- Avoid non-ascii ContentIdentifiers because fully arbitrary
	-- strings may not be encoded using the filesystem
	-- encoding, which is normally applied to all input.
	arbitrary = ContentIdentifier . encodeBS
		<$> arbitrary `suchThat` all isAscii

{- List of files that can be imported from a remote, each with some added
 - information. -}
data ImportableContents info = ImportableContents
	{ importableContents :: [(ImportLocation, info)]
	, importableHistory :: [ImportableContents info]
	-- ^ Used by remotes that support importing historical versions of
	-- files that are stored in them. This is equivalent to a git
	-- commit history.
	--
	-- When retrieving a historical version of a file,
	-- old ImportLocations from importableHistory are not used;
	-- the content is no longer expected to be present at those
	-- locations. So, if a remote does not support Key/Value access,
	-- it should not populate the importableHistory.
	}
	deriving (Show, Generic, Functor)

instance NFData info => NFData (ImportableContents info)

{- ImportableContents, but it can be chunked into subtrees to avoid
 - all needing to fit in memory at the same time. -}
data ImportableContentsChunkable m info
	= ImportableContentsComplete (ImportableContents info)
	-- ^ Used when not chunking
	| ImportableContentsChunked
		{ importableContentsChunk :: ImportableContentsChunk m info
		, importableHistoryComplete :: [ImportableContents info]
		-- ^ Chunking the history is not supported
		}
	deriving (Functor)

{- A chunk of ImportableContents, which is the entire content of a subtree
 - of the main tree. Nested subtrees are not allowed. -}
data ImportableContentsChunk m info = ImportableContentsChunk
	{ importableContentsSubDir :: ImportChunkSubDir
	, importableContentsSubTree :: [(OsPath, info)]
	-- ^ locations are relative to importableContentsSubDir
	, importableContentsNextChunk :: m (Maybe (ImportableContentsChunk m info))
	-- ^ Continuation to get the next chunk.
	-- Returns Nothing when there are no more chunks.
	}
	deriving (Functor)

newtype ImportChunkSubDir = ImportChunkSubDir { importChunkSubDir :: OsPath }

importableContentsChunkFullLocation
	:: ImportChunkSubDir
	-> OsPath
	-> ImportLocation
importableContentsChunkFullLocation (ImportChunkSubDir root) loc =
#ifdef WITH_OSPATH
	mkImportLocation $ toOsPath $ getPosixString $ Posix.combine 
		(PosixString $ fromOsPath root)
		(PosixString $ fromOsPath loc)
#else
	mkImportLocation $ Posix.combine root loc
#endif
