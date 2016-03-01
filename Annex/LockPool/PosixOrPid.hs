{- Wraps Utility.LockPool, making pid locks be used when git-annex is so
 - configured.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
import Annex.Locations
import qualified Annex
import qualified Utility.LockPool.Posix as Posix
import qualified Utility.LockPool.PidLock as Pid
import qualified Utility.LockPool.LockHandle as H
import Utility.LockPool.LockHandle (LockHandle, dropLock)
import Utility.LockFile.Posix (openLockFile)
import Utility.LockPool.STM (LockFile)
import Utility.LockFile.LockStatus

import System.Posix

lockShared :: Maybe FileMode -> LockFile -> Annex LockHandle
lockShared m f = pidLock m f $ Posix.lockShared m f

lockExclusive :: Maybe FileMode -> LockFile -> Annex LockHandle
lockExclusive m f = pidLock m f $ Posix.lockExclusive m f

tryLockShared :: Maybe FileMode -> LockFile -> Annex (Maybe LockHandle)
tryLockShared m f = tryPidLock m f $ Posix.tryLockShared m f

tryLockExclusive :: Maybe FileMode -> LockFile -> Annex (Maybe LockHandle)
tryLockExclusive m f = tryPidLock m f $ Posix.tryLockExclusive m f

checkLocked :: LockFile -> Annex (Maybe Bool)
checkLocked f = Posix.checkLocked f `pidLockCheck` checkpid
  where
	checkpid pidlock = do
		v <- Pid.checkLocked pidlock
		case v of
			-- Only return true when the posix lock file exists.
			Just _ -> Posix.checkLocked f
			Nothing -> return Nothing

getLockStatus :: LockFile -> Annex LockStatus
getLockStatus f = Posix.getLockStatus f
	`pidLockCheck` Pid.getLockStatus

checkSaneLock :: LockFile -> LockHandle -> Annex Bool
checkSaneLock f h = H.checkSaneLock f h
	`pidLockCheck` flip Pid.checkSaneLock h

pidLockFile :: Annex (Maybe FilePath)
pidLockFile = ifM (annexPidLock <$> Annex.getGitConfig)
	( Just <$> Annex.fromRepo gitAnnexPidLockFile
	, pure Nothing
	)

pidLockCheck :: IO a -> (LockFile -> IO a) -> Annex a
pidLockCheck posixcheck pidcheck = 
	liftIO . maybe posixcheck pidcheck =<< pidLockFile

pidLock :: Maybe FileMode -> LockFile -> IO LockHandle -> Annex LockHandle
pidLock m f posixlock = go =<< pidLockFile
  where
	go Nothing = liftIO posixlock
	go (Just pidlock) = do
		timeout <- annexPidLockTimeout <$> Annex.getGitConfig
		liftIO $ do
			dummyPosixLock m f
			Pid.waitLock timeout pidlock

tryPidLock :: Maybe FileMode -> LockFile -> IO (Maybe LockHandle) -> Annex (Maybe LockHandle)
tryPidLock m f posixlock = liftIO . go =<< pidLockFile
  where
	go Nothing = posixlock
	go (Just pidlock) = do
		dummyPosixLock m f
		Pid.tryLock pidlock

-- The posix lock file is created even when using pid locks, in order to
-- avoid complicating any code that might expect to be able to see that
-- lock file.
dummyPosixLock :: Maybe FileMode -> LockFile -> IO ()
dummyPosixLock m f = closeFd =<< openLockFile ReadLock m f
