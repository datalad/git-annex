{- Posix lock files
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockFile.Posix (
	LockHandle,
	lockShared,
	lockExclusive,
	tryLockExclusive,
	checkLocked,
	getLockStatus,
	LockStatus(..),
	dropLock,
	checkSaneLock,
) where

import Utility.Exception
import Utility.Applicative

import System.IO
import System.Posix
import Data.Maybe

type LockFile = FilePath

newtype LockHandle = LockHandle Fd

-- Takes a shared lock, blocking until the lock is available.
lockShared :: Maybe FileMode -> LockFile -> IO LockHandle
lockShared = lock ReadLock

-- Takes an exclusive lock, blocking until the lock is available.
lockExclusive :: Maybe FileMode -> LockFile -> IO LockHandle
lockExclusive = lock WriteLock

-- Tries to take an exclusive lock, but does not block.
tryLockExclusive :: Maybe FileMode -> LockFile -> IO (Maybe LockHandle)
tryLockExclusive mode lockfile = do
	l <- openLockFile WriteLock mode lockfile
	v <- tryIO $ setLock l (WriteLock, AbsoluteSeek, 0, 0)
	case v of
		Left _ -> do
			closeFd l
			return Nothing
		Right _ -> return $ Just $ LockHandle l

-- Setting the FileMode allows creation of a new lock file.
-- If it's Nothing then this only succeeds when the lock file already exists.
lock :: LockRequest -> Maybe FileMode -> LockFile -> IO LockHandle
lock lockreq mode lockfile = do
	l <- openLockFile lockreq mode lockfile
	waitToSetLock l (lockreq, AbsoluteSeek, 0, 0)
	return (LockHandle l)

-- Close on exec flag is set so child processes do not inherit the lock.
openLockFile :: LockRequest -> Maybe FileMode -> LockFile -> IO Fd
openLockFile lockreq filemode lockfile = do
	l <- openFd lockfile openfor filemode defaultFileFlags
	setFdOption l CloseOnExec True
	return l
  where
	openfor = case lockreq of
		ReadLock -> ReadOnly
		_ -> ReadWrite

-- Returns Nothing when the file doesn't exist, for cases where
-- that is different from it not being locked.
checkLocked :: LockFile -> IO (Maybe Bool)
checkLocked = maybe Nothing (Just . isJust) <$$> getLockStatus'

data LockStatus = StatusUnLocked | StatusLockedBy ProcessID | StatusNoLockFile
	deriving (Eq)

getLockStatus :: LockFile -> IO LockStatus
getLockStatus lockfile = do
	v <- getLockStatus' lockfile
	return $ case v of
		Nothing -> StatusNoLockFile
		Just Nothing -> StatusUnLocked
		Just (Just pid) -> StatusLockedBy pid

getLockStatus' :: LockFile -> IO (Maybe (Maybe ProcessID))
getLockStatus' lockfile = go =<< catchMaybeIO open
  where
	open = openLockFile ReadLock Nothing lockfile
	go Nothing = return Nothing
	go (Just h) = do
		v <- getLock h (ReadLock, AbsoluteSeek, 0, 0)
		closeFd h
		return (Just (fmap fst v))

dropLock :: LockHandle -> IO ()
dropLock (LockHandle fd) = closeFd fd

-- Checks that the lock file still exists, and is the same file that was
-- locked to get the LockHandle.
--
-- This check is useful if the lock file might get deleted by something
-- else.
checkSaneLock :: LockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle fd) =
	go =<< catchMaybeIO (getFileStatus lockfile)
  where
	go Nothing = return False
	go (Just st) = do
		fdst <- getFdStatus fd
		return $ deviceID fdst == deviceID st && fileID fdst == fileID st
