{- git-annex program path
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Path where

import Annex.Common
import Config.Files
import Utility.Env
import Annex.PidLock
import qualified Annex

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
			else fromMaybe exe <$> readProgramFile
		maybe cannotFindProgram return =<< searchPath p

{- Returns the path for git-annex that is recorded in the programFile. -}
readProgramFile :: IO (Maybe FilePath)
readProgramFile = do
	programfile <- programFile
	headMaybe . lines <$> readFile programfile

cannotFindProgram :: IO a
cannotFindProgram = do
	f <- programFile
	giveup $ "cannot find git-annex program in PATH or in " ++ f

{- Runs a git-annex child process.
 -
 - Like runsGitAnnexChildProcessViaGit, when pid locking is in use,
 - this takes the pid lock, while running it, and sets an env var
 - that prevents the child process trying to take the pid lock,
 - to avoid it deadlocking.
 -}
gitAnnexChildProcess
	:: String
	-> [CommandParam]
	-> (CreateProcess -> CreateProcess)
	-> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
	-> Annex a
gitAnnexChildProcess subcmd ps f a = do
	cmd <- liftIO programPath
	ps' <- gitAnnexChildProcessParams subcmd ps
	pidLockChildProcess cmd ps' f a

{- Parameters to pass to a git-annex child process to run a subcommand
 - with some parameters.
 -
 - Includes -c values that were passed on the git-annex command line.
 -}
gitAnnexChildProcessParams :: String -> [CommandParam] -> Annex [CommandParam]
gitAnnexChildProcessParams subcmd ps = do
	cps <- concatMap (\c -> [Param "-c", Param c]) <$> Annex.getGitConfigOverrides
	return (Param subcmd : cps ++ ps)
