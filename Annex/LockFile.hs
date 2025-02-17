{- git-annex lock files.
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.LockFile (
	lockFileCached,
	unlockFile,
	getLockCache,
	fromLockCache,
	withSharedLock,
	withExclusiveLock,
	takeExclusiveLock,
	tryExclusiveLock,
	trySharedLock,
) where

import Annex.Common
import Annex
import Types.LockCache
import Annex.Perms
import Annex.LockPool

import qualified Data.Map as M

{- Create a specified lock file, and takes a shared lock, which is retained
 - in the cache. -}
lockFileCached :: OsPath -> Annex ()
lockFileCached file = go =<< fromLockCache file
  where
	go (Just _) = noop -- already locked
	go Nothing = do
#ifndef mingw32_HOST_OS
		mode <- annexFileMode
		lockhandle <- lockShared (Just mode) file
#else
		lockhandle <- liftIO $ waitToLock $ lockShared file
#endif
		changeLockCache $ M.insert file lockhandle

unlockFile :: OsPath -> Annex ()
unlockFile file = maybe noop go =<< fromLockCache file
  where
	go lockhandle = do
		liftIO $ dropLock lockhandle
		changeLockCache $ M.delete file

getLockCache :: Annex LockCache
getLockCache = getState lockcache

fromLockCache :: OsPath -> Annex (Maybe LockHandle)
fromLockCache file = M.lookup file <$> getLockCache

changeLockCache :: (LockCache -> LockCache) -> Annex ()
changeLockCache a = do
	m <- getLockCache
	changeState $ \s -> s { lockcache = a m }

{- Runs an action with a shared lock held. If an exclusive lock is held,
 - blocks until it becomes free. -}
withSharedLock :: OsPath -> Annex a -> Annex a
withSharedLock lockfile a = debugLocks $ do
	createAnnexDirectory $ takeDirectory lockfile
	mode <- annexFileMode
	bracket (lock mode lockfile) (liftIO . dropLock) (const a)
  where
#ifndef mingw32_HOST_OS
	lock mode = lockShared (Just mode)
#else
	lock _mode = liftIO . waitToLock . lockShared
#endif

{- Runs an action with an exclusive lock held. If the lock is already
 - held, blocks until it becomes free. -}
withExclusiveLock :: OsPath -> Annex a -> Annex a
withExclusiveLock lockfile a = bracket
	(takeExclusiveLock lockfile)
	(liftIO . dropLock)
	(const a)

{- Takes an exclusive lock, blocking until it's free. -}
takeExclusiveLock :: OsPath -> Annex LockHandle
takeExclusiveLock lockfile = debugLocks $ do
	createAnnexDirectory $ takeDirectory lockfile
	mode <- annexFileMode
	lock mode lockfile
  where
#ifndef mingw32_HOST_OS
	lock mode = lockExclusive (Just mode)
#else
	lock _mode = liftIO . waitToLock . lockExclusive
#endif

{- Tries to take an exclusive lock and run an action. If the lock is
 - already held, returns Nothing. -}
tryExclusiveLock :: OsPath -> Annex a -> Annex (Maybe a)
tryExclusiveLock lockfile a = debugLocks $ do
	createAnnexDirectory $ takeDirectory lockfile
	mode <- annexFileMode
	bracket (lock mode lockfile) (liftIO . unlock) go
  where
#ifndef mingw32_HOST_OS
	lock mode = tryLockExclusive (Just mode)
#else
	lock _mode = liftIO . lockExclusive
#endif
	unlock = maybe noop dropLock
	go Nothing = return Nothing
	go (Just _) = Just <$> a

{- Tries to take a shared lock, without blocking.
 -
 - Does not create the lock directory or lock file if it does not exist,
 - taking an exclusive lock will create them.
 -}
trySharedLock :: OsPath -> Annex (Maybe LockHandle)
trySharedLock lockfile = debugLocks $
#ifndef mingw32_HOST_OS
	tryLockShared Nothing lockfile
#else
	liftIO $ lockShared lockfile
#endif
