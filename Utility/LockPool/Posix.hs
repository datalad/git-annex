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

import System.IO
import System.Posix
import Data.Maybe
import Control.Applicative
import Prelude

-- Takes a shared lock, blocking until the lock is available.
lockShared :: Maybe FileMode -> LockFile -> IO LockHandle
lockShared mode file = makeLockHandle
	(P.waitTakeLock P.lockPool file LockShared)
	(mk <$> F.lockShared mode file)

-- Takes an exclusive lock, blocking until the lock is available.
lockExclusive :: Maybe FileMode -> LockFile -> IO LockHandle
lockExclusive mode file = makeLockHandle
	(P.waitTakeLock P.lockPool file LockExclusive)
	(mk <$> F.lockExclusive mode file)

-- Tries to take a shared lock, but does not block.
tryLockShared :: Maybe FileMode -> LockFile -> IO (Maybe LockHandle)
tryLockShared mode file = tryMakeLockHandle
	(P.tryTakeLock P.lockPool file LockShared)
	(fmap mk <$> F.tryLockShared mode file)

-- Tries to take an exclusive lock, but does not block.
tryLockExclusive :: Maybe FileMode -> LockFile -> IO (Maybe LockHandle)
tryLockExclusive mode file = tryMakeLockHandle
	(P.tryTakeLock P.lockPool file LockExclusive)
	(fmap mk <$> F.tryLockExclusive mode file)

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

checkSaneLock :: LockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle _ flo) = fCheckSaneLock flo lockfile

mk :: F.LockHandle -> FileLockOps
mk h = FileLockOps
	{ fDropLock = F.dropLock h
	, fCheckSaneLock = \f -> F.checkSaneLock f h
	}
