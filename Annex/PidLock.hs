{- Pid locking support.
 -
 - Copyright 2014-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.PidLock where

import Annex.Common
import Git
#ifndef mingw32_HOST_OS
import Git.Env
import Annex.GitOverlay
import qualified Utility.LockFile.PidLock as PidF
import qualified Utility.LockPool.PidLock as PidP
import Utility.LockPool (dropLock)
import Utility.Env
import Config
#endif

{- When pid locking is in use, this tries to take the pid lock (unless
 - the process already has it), and if successful, holds it while
 - running the child process. The child process is run with an env var
 - set, which prevents it from trying to take the pid lock itself.
 -
 - This way, any locking the parent does will not get in the way of
 - the child. The child is assumed to not do any locking that conflicts
 - with the parent, but if it did happen to do that, it would be noticed
 - when git-annex is used without pid locking.
 -
 - If another process is already holding the pid lock, the child process
 - is still run, but without setting the env var, so it can try to take the
 - pid lock itself, and fail however is appropriate for it in that
 - situation.
 -}
pidLockChildProcess
	:: FilePath
	-> [CommandParam]
	-> (CreateProcess -> CreateProcess)
	-> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
	-> Annex a
pidLockChildProcess cmd ps f a = do
	let p = f (proc cmd (toCommand ps))
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
	liftIO gonopidlock
#endif

{- Wrap around actions that may run a git-annex child process via a git
 - command.
 -
 - This is like pidLockChildProcess, but rather than running a process
 - itself, it runs the action with a modified Annex state that passes the
 - necessary env var when running git.
 -}
runsGitAnnexChildProcessViaGit :: Annex a -> Annex a
#ifndef mingw32_HOST_OS
runsGitAnnexChildProcessViaGit a = pidLockFile >>= \case
	Nothing -> a
	Just pidlock -> bracket (setup pidlock) cleanup (go pidlock)
  where
	setup pidlock = liftIO $ PidP.tryLock pidlock
	
	cleanup (Just h) = liftIO $ dropLock h
	cleanup Nothing = return ()
	
	go _ Nothing = a
	go pidlock (Just _h) = do
		v <- liftIO $ PidF.pidLockEnv pidlock
		let addenv g = do
			g' <- liftIO $ addGitEnv g v PidF.pidLockEnvValue
			return (g', ())
		let rmenv oldg g
			| any (\(k, _) -> k == v) (fromMaybe [] (Git.gitEnv oldg)) = g
			| otherwise = 
				let e' = case Git.gitEnv g of
					Just e -> Just (delEntry v e)
					Nothing -> Nothing
				in g { Git.gitEnv = e' }
		withAltRepo addenv rmenv (const a)
#else
runsGitAnnexChildProcessViaGit a = a
#endif

runsGitAnnexChildProcessViaGit' :: Git.Repo -> (Git.Repo -> IO a) -> Annex a
#ifndef mingw32_HOST_OS
runsGitAnnexChildProcessViaGit' r a = pidLockFile >>= \case
	Nothing -> liftIO $ a r
	Just pidlock -> liftIO $ bracket (setup pidlock) cleanup (go pidlock)
  where
	setup pidlock = PidP.tryLock pidlock
	
	cleanup (Just h) = dropLock h
	cleanup Nothing = return ()
	
	go _ Nothing = a r
	go pidlock (Just _h) = do
		v <- PidF.pidLockEnv pidlock
		r' <- addGitEnv r v PidF.pidLockEnvValue
		a r'
#else
runsGitAnnexChildProcessViaGit' r a = liftIO $ a r
#endif
