{- git-annex export types
 -
 - Copyright 2017-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

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
import Utility.OsPath

import GHC.Generics
import Control.DeepSeq
#ifdef WITH_OSPATH
import qualified System.OsPath.Posix as Posix
import System.OsString.Internal.Types
#else
import qualified System.FilePath.Posix as Posix
import Utility.FileSystemEncoding
#endif

-- A location such as a path on a remote, that a key can be exported to.
-- The path is relative to the top of the remote, and uses unix-style
-- path separators.
--
-- This must be a ShortByteString (which OsPath is) in order to to avoid
-- problems with ByteString getting PINNED in memory which caused memory
-- fragmentation and excessive memory use.
newtype ExportLocation = ExportLocation OsPath
	deriving (Show, Eq, Generic, Ord)

instance NFData ExportLocation

mkExportLocation :: OsPath -> ExportLocation
mkExportLocation = ExportLocation . toInternalGitPath

fromExportLocation :: ExportLocation -> OsPath
fromExportLocation (ExportLocation f) = f

newtype ExportDirectory = ExportDirectory OsPath
	deriving (Show, Eq)

mkExportDirectory :: OsPath -> ExportDirectory
mkExportDirectory = ExportDirectory . toInternalGitPath

fromExportDirectory :: ExportDirectory -> OsPath
fromExportDirectory (ExportDirectory f) = f

-- | All subdirectories down to the ExportLocation, with the deepest ones
-- last. Does not include the top of the export.
exportDirectories :: ExportLocation -> [ExportDirectory]
exportDirectories (ExportLocation f) =
	map (ExportDirectory . fromposixpath . Posix.joinPath . reverse)
		(subs [] dirs)
  where
	subs _ [] = []
	subs ps (d:ds) = (d:ps) : subs (d:ps) ds

#ifdef WITH_OSPATH
	dirs = map Posix.dropTrailingPathSeparator $
		dropFromEnd 1 $ Posix.splitPath $ PosixString $ fromOsPath f

	fromposixpath = toOsPath . getPosixString
#else
	dirs = map Posix.dropTrailingPathSeparator $
		dropFromEnd 1 $ Posix.splitPath $ fromOsPath f

	fromposixpath = encodeBS
#endif
