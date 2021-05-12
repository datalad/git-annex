{- git-annex program path
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Path (
	programPath,
	readProgramFile,
	gitAnnexChildProcess,
	gitAnnexChildProcessParams,
	gitAnnexDaemonizeParams,
) where

import Annex.Common
import Config.Files
import Utility.Env
import Annex.PidLock
import qualified Annex

import System.Environment (getExecutablePath, getArgs)

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
 - Includes -c values that were passed on the git-annex command line
 - or due to options like --debug being enabled.
 -}
gitAnnexChildProcessParams :: String -> [CommandParam] -> Annex [CommandParam]
gitAnnexChildProcessParams subcmd ps = do
	cps <- gitAnnexGitConfigOverrides
	return (Param subcmd : cps ++ ps)

gitAnnexGitConfigOverrides :: Annex [CommandParam]
gitAnnexGitConfigOverrides = concatMap (\c -> [Param "-c", Param c])
	<$> Annex.getGitConfigOverrides

{- Parameters to pass to git-annex when re-running the current command
 - to daemonize it. Used with Utility.Daemon.daemonize. -}
gitAnnexDaemonizeParams :: Annex [CommandParam]
gitAnnexDaemonizeParams = do
	-- This inclues -c parameters passed to git, as well as ones
	-- passed to git-annex.
	cps <- gitAnnexGitConfigOverrides
	-- Get every parameter git-annex was run with.
	ps <- liftIO getArgs
	return (map Param ps ++ cps)
