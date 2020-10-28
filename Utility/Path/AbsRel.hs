{- absolute and relative path manipulation
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Path.AbsRel (
	absPathFrom,
	absPath,
	relPathCwdToFile,
	relPathDirToFile,
	relPathDirToFileAbs,
	relHome,
) where

import System.FilePath.ByteString
#ifdef mingw32_HOST_OS
import System.Directory (getCurrentDirectory)
#else
import System.Posix.Directory.ByteString (getWorkingDirectory)
#endif
import Control.Applicative
import Prelude

import Utility.Path
import Utility.UserInfo
import Utility.FileSystemEncoding

{- Makes a path absolute.
 -
 - Also simplifies it using simplifyPath.
 -
 - The first parameter is a base directory (ie, the cwd) to use if the path
 - is not already absolute, and should itsef be absolute.
 -
 - Does not attempt to deal with edge cases or ensure security with
 - untrusted inputs.
 -}
absPathFrom :: RawFilePath -> RawFilePath -> RawFilePath
absPathFrom dir path = simplifyPath (combine dir path)

{- Converts a filename into an absolute path.
 -
 - Also simplifies it using simplifyPath.
 -
 - Unlike Directory.canonicalizePath, this does not require the path
 - already exists. -}
absPath :: RawFilePath -> IO RawFilePath
absPath file
	-- Avoid unncessarily getting the current directory when the path
	-- is already absolute. absPathFrom uses simplifyPath
	-- so also used here for consistency.
	| isAbsolute file = return $ simplifyPath file
	| otherwise = do
#ifdef mingw32_HOST_OS
		cwd <- toRawFilePath <$> getCurrentDirectory
#else
		cwd <- getWorkingDirectory
#endif
		return $ absPathFrom cwd file

{- Constructs a relative path from the CWD to a file.
 -
 - For example, assuming CWD is /tmp/foo/bar:
 -    relPathCwdToFile "/tmp/foo" == ".."
 -    relPathCwdToFile "/tmp/foo/bar" == "" 
 -}
relPathCwdToFile :: RawFilePath -> IO RawFilePath
relPathCwdToFile f = do
#ifdef mingw32_HOST_OS
	c <- toRawFilePath <$> getCurrentDirectory
#else
	c <- getWorkingDirectory
#endif
	relPathDirToFile c f

{- Constructs a relative path from a directory to a file. -}
relPathDirToFile :: RawFilePath -> RawFilePath -> IO RawFilePath
relPathDirToFile from to = relPathDirToFileAbs <$> absPath from <*> absPath to

{- Converts paths in the home directory to use ~/ -}
relHome :: FilePath -> IO String
relHome path = do
	let path' = toRawFilePath path
	home <- toRawFilePath <$> myHomeDir
	return $ if dirContains home path'
		then fromRawFilePath ("~/" <> relPathDirToFileAbs home path')
		else path
