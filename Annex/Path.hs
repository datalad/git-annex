{- git-annex program path
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Path where

import Common
import Config.Files
import Utility.Env

import System.Environment (getExecutablePath)

{- A fully qualified path to the currently running git-annex program.
 - 
 - getExecutablePath is used when possible. On OSs it supports
 - well, it returns the complete path to the program. But, on other OSs,
 - it might return just the basename. Fall back to reading the programFile,
 - or searching for the command name in PATH.
 -
 - The standalone build runs git-annex via ld.so, and defeats
 - getExecutablePath. It sets GIT_ANNEX_PROGRAMPATH to the correct path
 - to the wrapper script to use.
 -}
programPath :: IO FilePath
programPath = go =<< getEnv "GIT_ANNEX_PROGRAMPATH"
  where
	go (Just p) = return p
	go Nothing = do
		exe <- getExecutablePath
		p <- if isAbsolute exe
			then return exe
			else readProgramFile
		maybe cannotFindProgram return =<< searchPath p
