{- git-annex program path
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Path where

import Annex.Common
import Config.Files
import Utility.Env
#ifndef mingw32_HOST_OS
import qualified Utility.LockFile.PidLock as PidF
import qualified Utility.LockPool.PidLock as PidP
import Utility.LockPool (dropLock)
import Config
#endif

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
	:: [String]
	-> (CreateProcess -> CreateProcess)
	-> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
	-> Annex a
gitAnnexChildProcess ps f a = do
	cmd <- liftIO programPath
	let p = f (proc cmd ps)
	let gonopidlock = withCreateProcess p a
#ifndef mingw32_HOST_OS
	pidLockFile >>= liftIO . \case
		Nothing -> gonopidlock
		Just pidlock -> bracket
			(setup pidlock)
			cleanup
			(go gonopidlock p pidlock)
  where
  	setup pidlock = PidP.tryLock pidlock

	cleanup (Just h) = dropLock h
	cleanup Nothing = return ()

	go gonopidlock _ _ Nothing = gonopidlock
	go _ p pidlock (Just _h) = do
		v <- PidF.pidLockEnv pidlock
		baseenv <- case env p of
			Nothing -> getEnvironment
			Just baseenv -> pure baseenv
		let p' = p { env = Just ((v, PidF.pidLockEnvValue) : baseenv) }
		withCreateProcess p' a
#else
	gonopidlock
#endif
