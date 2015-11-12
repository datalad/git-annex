{- Handles for lock pools.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.LockPool.LockHandle where

import qualified Utility.LockPool.STM as P
import Utility.LockPool.STM (LockFile)

import Control.Concurrent.STM
import Control.Exception

data LockHandle = LockHandle
	{ poolHandle :: P.LockHandle
	, fileLockOps :: FileLockOps
	}

data FileLockOps = FileLockOps
	{ fDropLock :: IO ()
#ifndef mingw32_HOST_OS
	, fCheckSaneLock :: LockFile -> IO Bool
#endif
	}

dropLock :: LockHandle -> IO ()
dropLock h = P.releaseLock (poolHandle h) (fDropLock (fileLockOps h))

#ifndef mingw32_HOST_OS
checkSaneLock :: LockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle _ flo) = fCheckSaneLock flo lockfile
#endif

-- Take a lock, by first updating the lock pool, and then taking the file
-- lock. If taking the file lock fails for any reason, take care to
-- release the lock in the lock pool.
makeLockHandle :: STM P.LockHandle -> IO FileLockOps -> IO LockHandle
makeLockHandle pa fa = bracketOnError setup cleanup go
  where
	setup = atomically pa
	cleanup ph = P.releaseLock ph (return ())
	go ph = do
		fo <- fa
		return $ LockHandle ph fo

tryMakeLockHandle :: STM (Maybe P.LockHandle) -> IO (Maybe FileLockOps) -> IO (Maybe LockHandle)
tryMakeLockHandle pa fa = bracketOnError setup cleanup go
  where
	setup = atomically pa
	cleanup Nothing = return ()
	cleanup (Just ph) = P.releaseLock ph (return ())
	go Nothing = return Nothing
	go (Just ph) = do
		mfo <- fa
		case mfo of
			Nothing -> do
				cleanup (Just ph)
				return Nothing
			Just fo -> return $ Just $ LockHandle ph fo
