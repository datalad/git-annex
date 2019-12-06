{- git-annex import types
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Import where

import qualified Data.ByteString as S
import Data.Char

import Types.Export
import Utility.QuickCheck
import Utility.FileSystemEncoding

{- Location of content on a remote that can be imported. 
 - This is just an alias to ExportLocation, because both are referring to a
 - location on the remote. -}
type ImportLocation = ExportLocation

mkImportLocation :: RawFilePath -> ImportLocation
mkImportLocation = mkExportLocation

fromImportLocation :: ImportLocation -> RawFilePath
fromImportLocation = fromExportLocation

{- An identifier for content stored on a remote that has been imported into
 - the repository. It should be reasonably short since it is stored in the
 - git-annex branch. -}
newtype ContentIdentifier = ContentIdentifier S.ByteString
	deriving (Eq, Ord, Show)

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
	-- files that are stored in them. This is equivilant to a git
	-- commit history.
	}
	deriving (Show)
