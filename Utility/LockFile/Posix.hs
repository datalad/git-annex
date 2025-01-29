{- Posix lock files
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockFile.Posix (
	LockHandle,
	lockShared,
	lockExclusive,
	tryLockShared,
	tryLockExclusive,
	checkLocked,
	getLockStatus,
	LockStatus(..),
	dropLock,
	checkSaneLock,
	LockRequest(..),
	openLockFile,
) where

import Utility.Exception
import Utility.Applicative
import Utility.FileMode
import Utility.LockFile.LockStatus
import Utility.OpenFd
import Utility.OsPath

import System.IO
import System.Posix.Types
import System.Posix.IO.ByteString
import System.Posix.Files.ByteString
import Data.Maybe

type LockFile = OsPath

newtype LockHandle = LockHandle Fd

-- Takes a shared lock, blocking until the lock is available.
lockShared :: Maybe ModeSetter -> LockFile -> IO LockHandle
lockShared = lock ReadLock

-- Takes an exclusive lock, blocking until the lock is available.
lockExclusive :: Maybe ModeSetter -> LockFile -> IO LockHandle
lockExclusive = lock WriteLock

-- Tries to take a shared lock, but does not block.
tryLockShared :: Maybe ModeSetter -> LockFile -> IO (Maybe LockHandle)
tryLockShared = tryLock ReadLock

-- Tries to take an exclusive lock, but does not block.
tryLockExclusive :: Maybe ModeSetter -> LockFile -> IO (Maybe LockHandle)
tryLockExclusive = tryLock WriteLock

-- Setting the FileMode allows creation of a new lock file.
-- If it's Nothing then this only succeeds when the lock file already exists.
lock :: LockRequest -> Maybe ModeSetter -> LockFile -> IO LockHandle
lock lockreq mode lockfile = do
	l <- openLockFile lockreq mode lockfile
	waitToSetLock l (lockreq, AbsoluteSeek, 0, 0)
	return (LockHandle l)

-- Tries to take an lock, but does not block.
tryLock :: LockRequest -> Maybe ModeSetter -> LockFile -> IO (Maybe LockHandle)
tryLock lockreq mode lockfile = uninterruptibleMask_ $ do
	l <- openLockFile lockreq mode lockfile
	v <- tryIO $ setLock l (lockreq, AbsoluteSeek, 0, 0)
	case v of
		Left _ -> do
			closeFd l
			return Nothing
		Right _ -> return $ Just $ LockHandle l

-- Close on exec flag is set so child processes do not inherit the lock.
openLockFile :: LockRequest -> Maybe ModeSetter -> LockFile -> IO Fd
openLockFile lockreq filemode lockfile = do
	l <- applyModeSetter filemode lockfile $ \filemode' ->
		openFdWithMode (fromOsPath lockfile) openfor filemode' defaultFileFlags
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

getLockStatus :: LockFile -> IO LockStatus
getLockStatus lockfile = do
	v <- getLockStatus' lockfile
	return $ case v of
		Nothing -> StatusNoLockFile
		Just Nothing -> StatusUnLocked
		Just (Just pid) -> StatusLockedBy pid

getLockStatus' :: LockFile -> IO (Maybe (Maybe ProcessID))
getLockStatus' lockfile = bracket open close go
  where
	open = catchMaybeIO $ openLockFile ReadLock Nothing lockfile

	close (Just h) = closeFd h
	close Nothing = return ()

	go (Just h) = do
		v <- getLock h (ReadLock, AbsoluteSeek, 0, 0)
		return (Just (fmap fst v))
	go Nothing = return Nothing

dropLock :: LockHandle -> IO ()
dropLock (LockHandle fd) = closeFd fd

-- Checks that the lock file still exists, and is the same file that was
-- locked to get the LockHandle.
--
-- This check is useful if the lock file might get deleted by something
-- else.
checkSaneLock :: LockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle fd) =
	go =<< catchMaybeIO (getFileStatus (fromOsPath lockfile))
  where
	go Nothing = return False
	go (Just st) = do
		fdst <- getFdStatus fd
		return $ deviceID fdst == deviceID st && fileID fdst == fileID st
