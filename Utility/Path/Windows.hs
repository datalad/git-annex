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
import qualified Utility.OsString as OS
import Utility.SystemDirectory
import Utility.FileSystemEncoding

import qualified Data.ByteString as B
import qualified System.FilePath.Windows as WinPath

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
		cwd <- getCurrentDirectory
		let p = simplifyPath (combine cwd (toOsPath f))
		-- If the input path is absolute but does not include a drive,
		-- add the drive from the cwd, because a path in the native
		-- namespace must include a drive.
		let p' = if OS.null (takeDrive p)
			then joinDrive (takeDrive cwd) p
			else p
		-- Normalize slashes.
		let p'' = encodeBS $ WinPath.normalise $ fromOsPath p'
		return (win32_file_namespace <> p'')
  where
	win32_dev_namespace = "\\\\.\\"
	win32_file_namespace = "\\\\?\\"
	nt_device_namespace = "\\Device\\"
