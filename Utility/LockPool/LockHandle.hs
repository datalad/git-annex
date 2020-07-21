{- Handles for lock pools.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.LockPool.LockHandle (
	LockHandle,
	FileLockOps(..),
	dropLock,
#ifndef mingw32_HOST_OS
	checkSaneLock,
#endif
	makeLockHandle,
	tryMakeLockHandle,
) where

import qualified Utility.LockPool.STM as P
import Utility.LockPool.STM (LockFile)
import Utility.DebugLocks

import Control.Concurrent.STM
import Control.Exception
import Control.Applicative
import Prelude

data LockHandle = LockHandle P.LockHandle FileLockOps

data FileLockOps = FileLockOps
	{ fDropLock :: IO ()
#ifndef mingw32_HOST_OS
	, fCheckSaneLock :: LockFile -> IO Bool
#endif
	}

dropLock :: LockHandle -> IO ()
dropLock (LockHandle ph _) = P.releaseLock ph

#ifndef mingw32_HOST_OS
checkSaneLock :: LockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle _ flo) = fCheckSaneLock flo lockfile
#endif

-- Take a lock, by first updating the lock pool, and then taking the file
-- lock. If taking the file lock fails for any reason, take care to
-- release the lock in the lock pool.
makeLockHandle :: P.LockPool -> LockFile -> (P.LockPool -> LockFile -> STM P.LockHandle) -> (LockFile -> IO FileLockOps) -> IO LockHandle
makeLockHandle pool file pa fa = bracketOnError setup cleanup go
  where
	setup = debugLocks $ atomically (pa pool file)
	cleanup ph = debugLocks $ P.releaseLock ph
	go ph = mkLockHandle ph =<< fa file

tryMakeLockHandle :: P.LockPool -> LockFile -> (P.LockPool -> LockFile -> STM (Maybe P.LockHandle)) -> (LockFile -> IO (Maybe FileLockOps)) -> IO (Maybe LockHandle)
tryMakeLockHandle pool file pa fa = bracketOnError setup cleanup go
  where
	setup = atomically (pa pool file)
	cleanup Nothing = return ()
	cleanup (Just ph) = P.releaseLock ph
	go Nothing = return Nothing
	go (Just ph) = do
		mfo <- fa file
		case mfo of
			Nothing -> do
				cleanup (Just ph)
				return Nothing
			Just fo -> Just <$> mkLockHandle ph fo

mkLockHandle :: P.LockHandle -> FileLockOps -> IO LockHandle
mkLockHandle ph fo = do
	atomically $ P.registerCloseLockFile ph (fDropLock fo)
	return $ LockHandle ph fo
	
