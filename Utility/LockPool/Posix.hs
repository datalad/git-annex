{- Posix lock files, using lock pools.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockPool.Posix (
	P.LockFile,
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
) where

import qualified Utility.LockFile.Posix as F
import Utility.LockFile.LockStatus
import qualified Utility.LockPool.STM as P
import Utility.LockPool.STM (LockFile, LockMode(..))
import Utility.LockPool.LockHandle
import Utility.FileMode

import System.Posix

-- Takes a shared lock, blocking until the lock is available.
lockShared :: Maybe ModeSetter -> LockFile -> IO LockHandle
lockShared mode file = fst <$> makeLockHandle P.lockPool file
	(\p f -> P.waitTakeLock p f LockShared)
	(\f _ -> mk <$> F.lockShared mode f)

-- Takes an exclusive lock, blocking until the lock is available.
lockExclusive :: Maybe ModeSetter -> LockFile -> IO LockHandle
lockExclusive mode file = fst <$> makeLockHandle P.lockPool file
	(\p f -> P.waitTakeLock p f LockExclusive)
	(\f _ -> mk <$> F.lockExclusive mode f)

-- Tries to take a shared lock, but does not block.
tryLockShared :: Maybe ModeSetter -> LockFile -> IO (Maybe LockHandle)
tryLockShared mode file = fmap fst <$> tryMakeLockHandle P.lockPool file
	(\p f -> P.tryTakeLock p f LockShared)
	(\f _ -> fmap mk <$> F.tryLockShared mode f)

-- Tries to take an exclusive lock, but does not block.
tryLockExclusive :: Maybe ModeSetter -> LockFile -> IO (Maybe LockHandle)
tryLockExclusive mode file = fmap fst <$> tryMakeLockHandle P.lockPool file
	(\p f -> P.tryTakeLock p f LockExclusive)
	(\f _ -> fmap mk <$> F.tryLockExclusive mode f)

-- Returns Nothing when the file doesn't exist, for cases where
-- that is different from it not being locked.
checkLocked :: LockFile -> IO (Maybe Bool)
checkLocked file = P.getLockStatus P.lockPool file
	(pure (Just True))
	(F.checkLocked file)

getLockStatus :: LockFile -> IO LockStatus
getLockStatus file = P.getLockStatus P.lockPool file
	(StatusLockedBy <$> getProcessID)
	(F.getLockStatus file)

mk :: F.LockHandle -> (FileLockOps, ())
mk h = (FileLockOps
	{ fDropLock = F.dropLock h
	, fCheckSaneLock = \f -> F.checkSaneLock f h
	}, ())
