{- Wraps Utility.LockPool, making pid locks be used when git-annex is so
 - configured.
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.LockPool.PosixOrPid (
	LockFile,
	LockHandle,
	lockShared,
	lockExclusive,
	tryLockShared,
	tryLockExclusive,
	dropLock,
	checkLocked,
	LockStatus(..),
	getLockStatus,
	checkSaneLock,
) where

import Common
import Types
import qualified Annex
import qualified Utility.LockPool.Posix as Posix
import qualified Utility.LockPool.PidLock as Pid
import qualified Utility.LockPool.LockHandle as H
import Utility.FileMode
import Utility.LockPool.LockHandle (LockHandle, dropLock)
import Utility.LockFile.Posix (openLockFile)
import Utility.LockPool.STM (LockFile, LockMode(..))
import Utility.LockFile.LockStatus
import Config (pidLockFile)
import Messages (warning)
import Git.Quote

import System.Posix

lockShared :: Maybe ModeSetter -> LockFile -> Annex LockHandle
lockShared m f = pidLock m f LockShared $ Posix.lockShared m f

lockExclusive :: Maybe ModeSetter -> LockFile -> Annex LockHandle
lockExclusive m f = pidLock m f LockExclusive $ Posix.lockExclusive m f

tryLockShared :: Maybe ModeSetter -> LockFile -> Annex (Maybe LockHandle)
tryLockShared m f = tryPidLock m f LockShared $ Posix.tryLockShared m f

tryLockExclusive :: Maybe ModeSetter -> LockFile -> Annex (Maybe LockHandle)
tryLockExclusive m f = tryPidLock m f LockExclusive $ Posix.tryLockExclusive m f

checkLocked :: LockFile -> Annex (Maybe Bool)
checkLocked f = Posix.checkLocked f `pidLockCheck` checkpid
  where
	checkpid pidlock = Pid.checkLocked pidlock >>= \case
		-- Only return true when the posix lock file exists.
		Just _ -> Posix.checkLocked f
		Nothing -> return Nothing

getLockStatus :: LockFile -> Annex LockStatus
getLockStatus f = Posix.getLockStatus f
	`pidLockCheck` Pid.getLockStatus

checkSaneLock :: LockFile -> LockHandle -> Annex Bool
checkSaneLock f h = H.checkSaneLock f h
	`pidLockCheck` flip Pid.checkSaneLock h

pidLockCheck :: IO a -> (LockFile -> IO a) -> Annex a
pidLockCheck posixcheck pidcheck = debugLocks $
	liftIO . maybe posixcheck pidcheck =<< pidLockFile

pidLock :: Maybe ModeSetter -> LockFile -> LockMode -> IO LockHandle -> Annex LockHandle
pidLock m f lockmode posixlock = debugLocks $ go =<< pidLockFile
  where
	go Nothing = liftIO posixlock
	go (Just pidlock) = do
		timeout <- annexPidLockTimeout <$> Annex.getGitConfig
		liftIO $ dummyPosixLock m f
		Pid.waitLock f lockmode timeout pidlock (warning . UnquotedString)

tryPidLock :: Maybe ModeSetter -> LockFile -> LockMode -> IO (Maybe LockHandle) -> Annex (Maybe LockHandle)
tryPidLock m f lockmode posixlock = debugLocks $ liftIO . go =<< pidLockFile
  where
	go Nothing = posixlock
	go (Just pidlock) = do
		dummyPosixLock m f
		Pid.tryLock f lockmode pidlock

-- The posix lock file is created even when using pid locks, in order to
-- avoid complicating any code that might expect to be able to see that
-- lock file. But, it's not locked.
dummyPosixLock :: Maybe ModeSetter -> LockFile -> IO ()
dummyPosixLock m f = bracket (openLockFile ReadLock m f) closeFd (const noop)
