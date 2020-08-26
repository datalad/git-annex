{- Handles for lock pools.
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
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
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO, MonadIO)
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
makeLockHandle
	:: (MonadIO m, MonadMask m)
	=> P.LockPool
	-> LockFile
	-> (P.LockPool -> LockFile -> STM P.LockHandle)
	-> (LockFile -> m FileLockOps)
	-> m LockHandle
makeLockHandle pool file pa fa = bracketOnError setup cleanup go
  where
	setup = debugLocks $ liftIO $ atomically (pa pool file)
	cleanup ph = debugLocks $ liftIO $ P.releaseLock ph
	go ph = liftIO . mkLockHandle ph =<< fa file

tryMakeLockHandle
	:: (MonadIO m, MonadMask m)
	=> P.LockPool
	-> LockFile
	-> (P.LockPool -> LockFile -> STM (Maybe P.LockHandle))
	-> (LockFile -> m (Maybe FileLockOps))
	-> m (Maybe LockHandle)
tryMakeLockHandle pool file pa fa = bracketOnError setup cleanup go
  where
	setup = liftIO $ atomically (pa pool file)
	cleanup Nothing = return ()
	cleanup (Just ph) = liftIO $ P.releaseLock ph
	go Nothing = return Nothing
	go (Just ph) = do
		mfo <- fa file
		case mfo of
			Nothing -> do
				liftIO $ cleanup (Just ph)
				return Nothing
			Just fo -> liftIO $ Just <$> mkLockHandle ph fo

mkLockHandle :: P.LockHandle -> FileLockOps -> IO LockHandle
mkLockHandle ph fo = do
	atomically $ P.registerCloseLockFile ph (fDropLock fo)
	return $ LockHandle ph fo
	
