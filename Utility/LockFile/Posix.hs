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
	tryLockExclusive,
	createLockFile,
	openExistingLockFile,
	isLocked,
	checkLocked,
	dropLock,
) where

import Utility.Exception
import Utility.Applicative

import System.IO
import System.Posix
import Data.Maybe
import Control.Applicative

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
	l <- openLockFile mode lockfile
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
	l <- openLockFile mode lockfile
	waitToSetLock l (lockreq, AbsoluteSeek, 0, 0)
	return (LockHandle l)

-- Create and opens lock file; does not lock it.
createLockFile :: FileMode -> LockFile -> IO Fd
createLockFile filemode = openLockFile (Just filemode)

-- Opens an existing lock file; does not lock it, and if it does not exist,
-- returns Nothing.
openExistingLockFile :: LockFile -> IO (Maybe Fd)
openExistingLockFile = catchMaybeIO . openLockFile Nothing

-- Close on exec flag is set so child processes do not inherit the lock.
openLockFile :: Maybe FileMode -> LockFile -> IO Fd
openLockFile filemode lockfile = do
	l <- openFd lockfile ReadWrite filemode defaultFileFlags
	setFdOption l CloseOnExec True
	return l

-- Check if a file is locked, either exclusively, or with shared lock.
-- When the file doesn't exist, it's considered not locked.
isLocked :: LockFile -> IO Bool
isLocked = fromMaybe False <$$> checkLocked

checkLocked :: LockFile -> IO (Maybe Bool)
checkLocked lockfile = go =<< catchMaybeIO open
  where
	open = openFd lockfile ReadOnly Nothing defaultFileFlags
	go Nothing = return Nothing
	go (Just h) = do
		ret <- isJust <$> getLock h (ReadLock, AbsoluteSeek, 0, 0)
		closeFd h
		return $ Just ret

dropLock :: LockHandle -> IO ()
dropLock (LockHandle fd) = closeFd fd
