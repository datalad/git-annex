{- Windows paths
 -
 - Copyright 2022-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Path.Windows (
	convertToWindowsNativeNamespace
) where

import Utility.Path
import Utility.OsPath
import Utility.FileSystemEncoding

import qualified Data.ByteString as B
import qualified System.FilePath.Windows.ByteString as P
import System.Directory (getCurrentDirectory)

{- Convert a filepath to use Windows's native namespace.
 - This avoids filesystem length limits.
 -
 - This is similar to the way base converts filenames on windows,
 - but as that is implemented in C (create_device_name) and not
 - exported, it cannot be used here. Several edge cases are not handled,
 - including network shares and dos short paths.
 -}
convertToWindowsNativeNamespace :: RawFilePath -> IO RawFilePath
convertToWindowsNativeNamespace f
	| win32_dev_namespace `B.isPrefixOf` f = return f
	| win32_file_namespace `B.isPrefixOf` f = return f
	| nt_device_namespace `B.isPrefixOf` f = return f
	| otherwise = do
		-- Make absolute because any '.' and '..' in the path
		-- will not be resolved once it's converted.
		cwd <- toRawFilePath <$> getCurrentDirectory
		let p = fromOsPath (simplifyPath (toOsPath (combine cwd f)))
		-- Normalize slashes.
		let p' = P.normalise p
		return (win32_file_namespace <> p')
  where
	win32_dev_namespace = "\\\\.\\"
	win32_file_namespace = "\\\\?\\"
	nt_device_namespace = "\\Device\\"
