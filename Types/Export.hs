{- git-annex export types
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
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

import qualified System.FilePath.Posix as Posix
import GHC.Generics
import Control.DeepSeq

-- A location on a remote that a key can be exported to.
-- The RawFilePath will be relative to the top of the remote,
-- and uses unix-style path separators.
newtype ExportLocation = ExportLocation RawFilePath
	deriving (Show, Eq, Generic)

instance NFData ExportLocation

mkExportLocation :: RawFilePath -> ExportLocation
mkExportLocation = ExportLocation . toInternalGitPath

fromExportLocation :: ExportLocation -> RawFilePath
fromExportLocation (ExportLocation f) = f

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
		dropFromEnd 1 $ Posix.splitPath $ decodeBS f
