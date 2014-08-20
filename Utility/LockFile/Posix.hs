{- Posix lock files
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

module Utility.LockFile.Posix (
	LockHandle,
	lockShared,
	lockExclusive,
	dropLock,
	createLockFile,
	openExistingLockFile,
) where

import Utility.Exception

import System.IO
import System.Posix

type LockFile = FilePath

newtype LockHandle = LockHandle Fd

-- Takes a shared lock, blocking until the lock is available.
lockShared :: Maybe FileMode -> LockFile -> IO LockHandle
lockShared = lock ReadLock

-- Takes an exclusive lock, blocking until the lock is available.
lockExclusive :: Maybe FileMode -> LockFile -> IO LockHandle
lockExclusive = lock WriteLock

-- The FileMode is used when creating a new lock file.
lock :: LockRequest -> Maybe FileMode -> LockFile -> IO LockHandle
lock lockreq mode lockfile = do
	l <- createLockFile mode lockfile
	waitToSetLock l (lockreq, AbsoluteSeek, 0, 0)
	return (LockHandle l)

-- Create and opens lock file; does not lock it.
createLockFile :: Maybe FileMode -> LockFile -> IO Fd
createLockFile = openLockFile ReadWrite

-- Opens an existing lock file; does not lock it or create it.
openExistingLockFile :: LockFile -> IO (Maybe Fd)
openExistingLockFile = catchMaybeIO . openLockFile ReadOnly Nothing

-- Close on exec flag is set so child processes do not inherit the lock.
openLockFile :: OpenMode -> Maybe FileMode -> LockFile -> IO Fd
openLockFile openmode filemode lockfile = do
	l <- openFd lockfile openmode filemode defaultFileFlags
	setFdOption l CloseOnExec True
	return l

dropLock :: LockHandle -> IO ()
dropLock (LockHandle fd) = closeFd fd
