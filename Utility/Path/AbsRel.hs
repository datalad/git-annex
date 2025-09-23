{- absolute and relative path manipulation
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Path.AbsRel (
	absPathFrom,
	absPath,
	relPathCwdToFile,
	relPathDirToFile,
	relPathDirToFileAbs,
	relHome,
) where

import qualified Data.ByteString as B

import Utility.Path
import Utility.UserInfo
import Utility.OsPath
import Utility.SystemDirectory

{- Makes a path absolute.
 -
 - Also simplifies it using simplifyPath.
 -
 - The first parameter is a base directory (ie, the cwd) to use if the path
 - is not already absolute, and should itself be absolute.
 -
 - Does not attempt to deal with edge cases or ensure security with
 - untrusted inputs.
 -}
absPathFrom :: OsPath -> OsPath -> OsPath
absPathFrom dir path = simplifyPath (combine dir path)

{- Converts a filename into an absolute path.
 -
 - Also simplifies it using simplifyPath.
 -
 - Unlike Directory.canonicalizePath, this does not require the path
 - already exists. -}
absPath :: OsPath -> IO OsPath
absPath file
	-- Avoid unnecessarily getting the current directory when the path
	-- is already absolute. absPathFrom uses simplifyPath
	-- so also used here for consistency.
	| isAbsolute file = return $ simplifyPath file
	| otherwise = do
		cwd <- getCurrentDirectory
		return $ absPathFrom cwd file

{- Constructs the minimal relative path from the CWD to a file.
 -
 - For example, assuming CWD is /tmp/foo/bar:
 -    relPathCwdToFile "/tmp/foo" == ".."
 -    relPathCwdToFile "/tmp/foo/bar" == "" 
 -    relPathCwdToFile "../bar/baz" == "baz"
 -}
relPathCwdToFile :: OsPath -> IO OsPath
relPathCwdToFile f
	-- Optimisation: Avoid doing any IO when the path is relative
	-- and does not contain any ".." component.
	| isRelative f && not (".." `B.isInfixOf` fromOsPath f) = return f
	| otherwise = do
		c <- getCurrentDirectory
		relPathDirToFile c f

{- Constructs a minimal relative path from a directory to a file. -}
relPathDirToFile :: OsPath -> OsPath -> IO OsPath
relPathDirToFile from to = relPathDirToFileAbs <$> absPath from <*> absPath to

{- Converts paths in the home directory to use ~/ -}
relHome :: OsPath -> IO OsPath
relHome path = do
	home <- toOsPath <$> myHomeDir
	return $ if dirContains home path
		then literalOsPath "~/" <> relPathDirToFileAbs home path
		else path
