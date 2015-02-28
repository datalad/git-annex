{- git-annex program path
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Path where

import Common
import Config.Files
import System.Environment

{- A fully qualified path to the currently running git-annex program.
 - 
 - getExecutablePath is available since ghc 7.4.2. On OSs it supports
 - well, it returns the complete path to the program. But, on other OSs,
 - it might return just the basename. Fall back to reading the programFile,
 - or searching for the command name in PATH.
 -}
programPath :: IO FilePath
programPath = do
#if MIN_VERSION_base(4,6,0)
	exe <- getExecutablePath
	p <- if isAbsolute exe
		then return exe
		else readProgramFile
#else
	p <- readProgramFile
#endif
	maybe cannotFindProgram return =<< searchPath p
