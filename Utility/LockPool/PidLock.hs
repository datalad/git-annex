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
-- or the Seconds timeout if the pid lock is held by another process.
--
-- There are two levels of locks. A STM lock is used to handle
-- fine-grained locking among threads, locking a specific lockfile,
-- but only in memory. The pid lock handles locking between processes.
--
-- The pid lock is only taken once, and LockShared is used for it,
-- so multiple threads can have it locked. Only the first thread
-- will create the pid lock, and it remains until all threads drop
-- their locks.
waitLock
	:: (MonadIO m, MonadMask m)
	=> LockFile
	-> LockMode
	-> Seconds
	-> F.PidLockFile
	-> (String -> m ())
	-> m LockHandle
waitLock finelockfile lockmode timeout pidlockfile displaymessage = do
	fl <- takefinelock
	pl <- takepidlock
		`onException` liftIO (dropLock fl)
	registerPostRelease fl pl
	return fl
  where
	takefinelock = fst <$> makeLockHandle P.lockPool finelockfile
		(\p f -> P.waitTakeLock p f lockmode)
		(\_ _ -> pure (stmonlyflo, ()))
	-- A shared STM lock is taken for each use of the pid lock,
	-- but only the first thread to take it actually creates the pid
	-- lock file.
	takepidlock = makeLockHandle P.lockPool pidlockfile
		(\p f -> P.waitTakeLock p f LockShared)
		(\f (P.FirstLock firstlock firstlocksem) -> if firstlock
			then waitlock f firstlocksem
			else liftIO (atomically $ readTMVar firstlocksem) >>= \case
				P.FirstLockSemWaited True -> alreadylocked f
				P.FirstLockSemTried True -> alreadylocked f
				P.FirstLockSemWaited False -> F.waitedLock timeout f displaymessage
				P.FirstLockSemTried False -> waitlock f firstlocksem
		)
	waitlock f firstlocksem = do
		h <- F.waitLock timeout f displaymessage $
			void . atomically . tryPutTMVar firstlocksem . P.FirstLockSemWaited
		return (mkflo h, Just h)
	alreadylocked f = do
		lh <- F.alreadyLocked f
		return (mkflo lh, Nothing)

registerPostRelease :: MonadIO m => LockHandle -> (LockHandle, Maybe F.LockHandle) -> m ()
registerPostRelease (LockHandle flh _) (pl@(LockHandle plh _), mpidlock) = do
	-- After the fine-grained lock gets dropped (and any shared locks
	-- of it are also dropped), drop the associated pid lock.
	liftIO $ atomically $
		P.registerPostReleaseLock flh (dropLock pl)
	-- When the last thread to use the pid lock has dropped it,
	-- close the pid lock file itself.
	case mpidlock of
		Just pidlock -> liftIO $ atomically $
			P.registerPostReleaseLock plh (F.dropLock pidlock)
		Nothing -> return ()

-- Tries to take a pid lock, but does not block.
tryLock :: LockFile -> LockMode -> F.PidLockFile -> IO (Maybe LockHandle)
tryLock finelockfile lockmode pidlockfile = takefinelock >>= \case
	Just fl -> tryLock' pidlockfile >>= \case
		Just pl -> do
			registerPostRelease fl pl
			return (Just fl)
		Nothing -> do
			dropLock fl
			return Nothing
	Nothing -> return Nothing
  where
	takefinelock = fmap fst <$> tryMakeLockHandle P.lockPool finelockfile
		(\p f -> P.tryTakeLock p f lockmode)
		(\_ _ -> pure (Just (stmonlyflo, ())))

tryLock' :: F.PidLockFile -> IO (Maybe (LockHandle, Maybe F.LockHandle))
tryLock' pidlockfile = tryMakeLockHandle P.lockPool pidlockfile
	(\p f -> P.tryTakeLock p f LockShared)
	(\f (P.FirstLock firstlock firstlocksem) -> if firstlock
		then do
			mlh <- F.tryLock f
			void $ atomically $ tryPutTMVar firstlocksem 
				(P.FirstLockSemTried (isJust mlh))
			case mlh of
				Just lh -> return (Just (mkflo lh, Just lh))
				Nothing -> return Nothing
		else liftIO (atomically $ readTMVar firstlocksem) >>= \case
			P.FirstLockSemWaited True -> alreadylocked f
			P.FirstLockSemTried True -> alreadylocked f
			P.FirstLockSemWaited False -> return Nothing
			P.FirstLockSemTried False -> return Nothing
	)
  where
	alreadylocked f = do
		lh <- F.alreadyLocked f
		return (Just (mkflo lh, Nothing))

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
	{ fDropLock = return ()
	, fCheckSaneLock = \f -> F.checkSaneLock f h
	}
		
stmonlyflo :: FileLockOps
stmonlyflo = FileLockOps
	{ fDropLock = return ()
	, fCheckSaneLock = const (return True)
	}
