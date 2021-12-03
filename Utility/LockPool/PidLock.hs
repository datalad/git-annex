{- Pid locks, using lock pools.
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockPool.PidLock (
	P.LockFile,
	LockHandle,
	waitLock,
	tryLock,
	tryLock',
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
import Control.Concurrent.STM
import Data.Maybe
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Applicative
import Prelude

-- Does locking using a pid lock, blocking until the lock is available
-- or the timeout.
--
-- There are two levels of locks. A STM lock is used to handle
-- fine-grained locking amoung threads, locking a specific lockfile,
-- but only in memory. The pid lock handles locking between processes.
--
-- The Seconds is how long to delay if the pid lock is held by another
-- process.
waitLock
	:: (MonadIO m, MonadMask m)
	=> LockFile
	-> LockMode
	-> Seconds
	-> F.PidLockFile
	-> (String -> m ())
	-> m LockHandle
waitLock stmlockfile lockmode timeout pidlockfile displaymessage = do
	sl@(LockHandle ph _) <- takestmlock
	pl <- takepidlock
	-- When the STM lock gets dropped, also drop the pid lock.
	liftIO $ atomically $
		P.registerPostReleaseLock ph (dropLock pl)
	return sl
  where
	takestmlock = makeLockHandle P.lockPool stmlockfile
		(\p f -> P.waitTakeLock p f lockmode)
		(\_ _ -> pure stmonlyflo)
	takepidlock = makeLockHandle P.lockPool pidlockfile
		-- LockShared because multiple threads can share the pid lock;
		-- it remains locked until all threads using it drop
		-- their locks.
		(\p f -> P.waitTakeLock p f LockShared)
		(\f (P.FirstLock firstlock firstlocksem) -> mkflo
			<$> if firstlock
				then F.waitLock timeout f displaymessage $
					void . atomically . tryPutTMVar firstlocksem . P.FirstLockSemWaited
				else liftIO (atomically $ readTMVar firstlocksem) >>= \case
					P.FirstLockSemWaited True -> F.alreadyLocked f
					P.FirstLockSemTried True -> F.alreadyLocked f
					P.FirstLockSemWaited False -> F.waitedLock timeout f displaymessage
					P.FirstLockSemTried False -> F.waitLock timeout f displaymessage $
						void . atomically . tryPutTMVar firstlocksem . P.FirstLockSemWaited
		)

-- Tries to take a pid lock, but does not block.
tryLock :: LockFile -> LockMode -> F.PidLockFile -> IO (Maybe LockHandle)
tryLock stmlockfile lockmode pidlockfile = takestmlock >>= \case
	Just (sl@(LockHandle ph _)) -> tryLock' pidlockfile >>= \case
		Just pl -> do
			liftIO $ atomically $
				P.registerPostReleaseLock ph (dropLock pl)
			return (Just sl)
		Nothing -> do
			dropLock sl
			return Nothing
	Nothing -> return Nothing
  where
	takestmlock = tryMakeLockHandle P.lockPool stmlockfile
		(\p f -> P.tryTakeLock p f lockmode)
		(\_ _ -> pure (Just stmonlyflo))

tryLock' :: F.PidLockFile -> IO (Maybe LockHandle)
tryLock' pidlockfile = tryMakeLockHandle P.lockPool pidlockfile
	(\p f -> P.tryTakeLock p f LockShared)
	(\f (P.FirstLock firstlock firstlocksem) -> fmap mkflo
		<$> if firstlock
			then do
				lh <- F.tryLock f
				void $ atomically $ tryPutTMVar firstlocksem 
					(P.FirstLockSemTried (isJust lh))
				return lh
			else liftIO (atomically $ readTMVar firstlocksem) >>= \case
					P.FirstLockSemWaited True -> Just <$> F.alreadyLocked f
					P.FirstLockSemTried True -> Just <$> F.alreadyLocked f
					P.FirstLockSemWaited False -> return Nothing
					P.FirstLockSemTried False -> return Nothing
	)

checkLocked :: LockFile -> IO (Maybe Bool)
checkLocked file = P.getLockStatus P.lockPool file
	(pure (Just True))
	(F.checkLocked file)

getLockStatus :: LockFile -> IO LockStatus
getLockStatus file = P.getLockStatus P.lockPool file
	(StatusLockedBy <$> getProcessID)
	(F.getLockStatus file)

mkflo :: F.LockHandle -> FileLockOps
mkflo h = FileLockOps
	{ fDropLock = F.dropLock h
	, fCheckSaneLock = \f -> F.checkSaneLock f h
	}
		
stmonlyflo :: FileLockOps
stmonlyflo = FileLockOps
	{ fDropLock = return ()
	, fCheckSaneLock = const (return True)
	}
