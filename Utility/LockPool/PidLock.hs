{- Pid locks, using lock pools.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockPool.PidLock (
	P.LockFile,
	LockHandle,
	waitLock,
	tryLock,
	checkLocked,
	getLockStatus,
	LockStatus(..),
	dropLock,
	checkSaneLock,
) where

import qualified Utility.LockFile.PidLock as F
import Utility.LockFile.LockStatus
import qualified Utility.LockPool.STM as P
import Utility.LockPool.STM (LockFile, LockMode(..))
import Utility.LockPool.LockHandle

import System.IO
import System.Posix
import Data.Maybe
import Control.Applicative
import Prelude

-- Takes a pid lock, blocking until the lock is available.
--
-- May block forever on stale locks, see PidLock documentation for details.
waitLock :: LockFile -> IO LockHandle
waitLock file = makeLockHandle
	(P.waitTakeLock P.lockPool file LockExclusive)
	(mk <$> F.waitLock file)

-- Tries to take a pid lock, but does not block.
tryLock :: LockFile -> IO (Maybe LockHandle)
tryLock file = tryMakeLockHandle
	(P.tryTakeLock P.lockPool file LockShared)
	(fmap mk <$> F.tryLock file)

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
