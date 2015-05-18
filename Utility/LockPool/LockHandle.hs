{- Handles for lock pools.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.LockPool.LockHandle where

import qualified Utility.LockPool.STM as P
#ifndef mingw32_HOST_OS
import qualified Utility.LockFile.Posix as F
#else
import qualified Utility.LockFile.Windows as F
#endif

import Control.Concurrent.STM
import Control.Exception

data LockHandle = LockHandle P.LockHandle F.LockHandle

dropLock :: LockHandle -> IO ()
dropLock (LockHandle ph fh) = P.releaseLock ph (F.dropLock fh)

-- Take a lock, by first updating the lock pool, and then taking the file
-- lock. If taking the file lock fails for any reason, take care to
-- release the lock in the lock pool.
makeLockHandle :: STM P.LockHandle -> IO F.LockHandle -> IO LockHandle
makeLockHandle pa fa = bracketOnError setup cleanup go
  where
	setup = atomically pa
	cleanup ph = P.releaseLock ph (return ())
	go ph = do
		fh <- fa
		return $ LockHandle ph fh

tryMakeLockHandle :: STM (Maybe P.LockHandle) -> IO (Maybe F.LockHandle) -> IO (Maybe LockHandle)
tryMakeLockHandle pa fa = bracketOnError setup cleanup go
  where
	setup = atomically pa
	cleanup Nothing = return ()
	cleanup (Just ph) = P.releaseLock ph (return ())
	go Nothing = return Nothing
	go (Just ph) = do
		mfh <- fa
		case mfh of
			Nothing -> do
				cleanup (Just ph)
				return Nothing
			Just fh -> return $ Just $ LockHandle ph fh
