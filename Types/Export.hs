{- git-annex export types
 -
 - Copyright 2017-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveGeneric #-}

module Types.Export (
	ExportLocation,
	mkExportLocation,
	fromExportLocation,
	ExportDirectory,
	mkExportDirectory,
	fromExportDirectory,
	exportDirectories,
) where

import Git.FilePath
import Utility.Split
import Utility.FileSystemEncoding

import qualified Data.ByteString.Short as S
import qualified System.FilePath.Posix as Posix
import GHC.Generics
import Control.DeepSeq

-- A location such as a path on a remote, that a key can be exported to.
-- The path is relative to the top of the remote, and uses unix-style
-- path separators.
--
-- This uses a ShortByteString to avoid problems with ByteString getting
-- PINNED in memory which caused memory fragmentation and excessive memory
-- use.
newtype ExportLocation = ExportLocation S.ShortByteString
	deriving (Show, Eq, Generic, Ord)

instance NFData ExportLocation

mkExportLocation :: RawFilePath -> ExportLocation
mkExportLocation = ExportLocation . S.toShort . toInternalGitPath

fromExportLocation :: ExportLocation -> RawFilePath
fromExportLocation (ExportLocation f) = S.fromShort f

newtype ExportDirectory = ExportDirectory RawFilePath
	deriving (Show, Eq)

mkExportDirectory :: RawFilePath -> ExportDirectory
mkExportDirectory = ExportDirectory . toInternalGitPath

fromExportDirectory :: ExportDirectory -> RawFilePath
fromExportDirectory (ExportDirectory f) = f

-- | All subdirectories down to the ExportLocation, with the deepest ones
-- last. Does not include the top of the export.
exportDirectories :: ExportLocation -> [ExportDirectory]
exportDirectories (ExportLocation f) =
	map (ExportDirectory . encodeBS . Posix.joinPath . reverse) (subs [] dirs)
  where
	subs _ [] = []
	subs ps (d:ds) = (d:ps) : subs (d:ps) ds

	dirs = map Posix.dropTrailingPathSeparator $
		dropFromEnd 1 $ Posix.splitPath $ decodeBS $ S.fromShort f
