{- git-annex export types
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Export where

import qualified System.FilePath.Posix as Posix

-- A location on a remote that a key can be exported to.
-- The FilePath will be relative to the top of the export,
-- and may contain unix-style path separators.
newtype ExportLocation = ExportLocation FilePath
	deriving (Show, Eq)

newtype ExportDirectory = ExportDirectory FilePath
	deriving (Show, Eq)

-- | All subdirectories down to the ExportLocation, with the deepest ones
-- last. Does not include the top of the export.
exportDirectories :: ExportLocation -> [ExportDirectory]
exportDirectories (ExportLocation f) =
	map (ExportDirectory . Posix.joinPath . reverse) (subs [] dirs)
  where
	subs _ [] = []
	subs ps (d:ds) = (d:ps) : subs (d:ps) ds

	dirs = map Posix.dropTrailingPathSeparator $
		reverse $ drop 1 $ reverse $ Posix.splitPath f
