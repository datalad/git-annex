{- STM implementation of lock pools.
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockPool.STM (
	LockPool,
	lockPool,
	LockFile,
	LockMode(..),
	LockHandle,
	waitTakeLock,
	tryTakeLock,
	getLockStatus,
	releaseLock,
	CloseLockFile,
	registerCloseLockFile,
) where

import Utility.Monad

import System.IO.Unsafe (unsafePerformIO)
import System.FilePath.ByteString (RawFilePath)
import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Applicative
import Prelude

type LockFile = RawFilePath

data LockMode = LockExclusive | LockShared
	deriving (Eq)

-- This TMVar is full when the handle is open, and is emptied when it's
-- closed.
type LockHandle = TMVar (LockPool, LockFile, CloseLockFile)

type LockCount = Integer

data LockStatus = LockStatus LockMode LockCount

type CloseLockFile = IO ()

-- This TMVar is normally kept full.
type LockPool = TMVar (M.Map LockFile LockStatus)

-- A shared global variable for the lockPool. Avoids callers needing to
-- maintain state for this implementation detail.
{-# NOINLINE lockPool #-}
lockPool :: LockPool
lockPool = unsafePerformIO (newTMVarIO M.empty)

-- Updates the LockPool, blocking as necessary if another thread is holding
-- a conflicting lock.
-- 
-- Note that when a shared lock is held, an exclusive lock will block.
-- While that blocking is happening, another call to this function to take
-- the same shared lock should not be blocked on the exclusive lock.
-- Keeping the whole Map in a TMVar accomplishes this, at the expense of
-- sometimes retrying after unrelated changes in the map.
waitTakeLock :: LockPool -> LockFile -> LockMode -> STM LockHandle
waitTakeLock pool file mode = maybe retry return =<< tryTakeLock pool file mode

-- Avoids blocking if another thread is holding a conflicting lock.
tryTakeLock :: LockPool -> LockFile -> LockMode -> STM (Maybe LockHandle)
tryTakeLock pool file mode = do
	m <- takeTMVar pool
	let success v = do
		putTMVar pool (M.insert file v m)
		Just <$> newTMVar (pool, file, noop)
	case M.lookup file m of
		Just (LockStatus mode' n)
			| mode == LockShared && mode' == LockShared ->
				success $ LockStatus mode (succ n)
			| n > 0 -> do
				putTMVar pool m
				return Nothing
		_ -> success $ LockStatus mode 1

-- Call after waitTakeLock or tryTakeLock, to register a CloseLockFile
-- action to run when releasing the lock.
registerCloseLockFile :: LockHandle -> CloseLockFile -> STM ()
registerCloseLockFile h closelockfile = do
	(p, f, c) <- takeTMVar h
	putTMVar h (p, f, c >> closelockfile)

-- Checks if a lock is being held. If it's held by the current process,
-- runs the getdefault action; otherwise runs the checker action.
--
-- Note that the lock pool is left empty while the checker action is run.
-- This allows checker actions that open/close files, and so would be in
-- danger of conflicting with locks created at the same time this is
-- running. With the lock pool empty, anything that attempts
-- to take a lock will block, avoiding that race.
getLockStatus :: LockPool -> LockFile -> IO v -> IO v -> IO v
getLockStatus pool file getdefault checker = do
	v <- atomically $ do
		m <- takeTMVar pool
		let threadlocked = case M.lookup file m of
			Just (LockStatus _ n) | n > 0 -> True
			_ -> False
		if threadlocked
			then do
				putTMVar pool m
				return Nothing
			else return $ Just $ atomically $ putTMVar pool m
	case v of
		Nothing -> getdefault
		Just restore -> bracket_ (return ()) restore checker

-- Only runs action to close underlying lock file when this is the last
-- user of the lock, and when the lock has not already been closed.
--
-- Note that the lock pool is left empty while the CloseLockFile action
-- is run, to avoid race with another thread trying to open the same lock
-- file.
releaseLock :: LockHandle -> IO ()
releaseLock h = go =<< atomically (tryTakeTMVar h)
  where
	go (Just (pool, file, closelockfile)) = do
		m <- atomically $ do
			m <- takeTMVar pool
			return $ case M.lookup file m of
				Just (LockStatus mode n)
					| n == 1 -> (M.delete file m)
					| otherwise ->
						(M.insert file (LockStatus mode (pred n)) m)
				Nothing -> m
		() <- closelockfile
		atomically $ putTMVar pool m
	-- The LockHandle was already closed.
	go Nothing = return ()
