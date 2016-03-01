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
import Utility.ThreadScheduler

import System.IO
import System.Posix
import Data.Maybe
import Control.Applicative
import Prelude

-- Takes a pid lock, blocking until the lock is available or the timeout.
waitLock :: Seconds -> LockFile -> IO LockHandle
waitLock timeout file = makeLockHandle
	-- LockShared for STM lock, because a pid lock can be the top-level
	-- lock with various other STM level locks gated behind it.
	(P.waitTakeLock P.lockPool file LockShared)
	(mk <$> F.waitLock timeout file)

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

mk :: F.LockHandle -> FileLockOps
mk h = FileLockOps
	{ fDropLock = F.dropLock h
	, fCheckSaneLock = \f -> F.checkSaneLock f h
	}
