{- git-annex lock files.
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.LockFile (
	lockFileCached,
	unlockFile,
	getLockCache,
	withExclusiveLock,
	tryExclusiveLock,
) where

import Annex.Common
import Annex
import Types.LockCache
import qualified Git
import Annex.Perms
import Annex.LockPool

import qualified Data.Map as M

{- Create a specified lock file, and takes a shared lock, which is retained
 - in the cache. -}
lockFileCached :: FilePath -> Annex ()
lockFileCached file = go =<< fromLockCache file
  where
	go (Just _) = noop -- already locked
	go Nothing = do
#ifndef mingw32_HOST_OS
		mode <- annexFileMode
		lockhandle <- noUmask mode $ lockShared (Just mode) file
#else
		lockhandle <- liftIO $ waitToLock $ lockShared file
#endif
		changeLockCache $ M.insert file lockhandle

unlockFile :: FilePath -> Annex ()
unlockFile file = maybe noop go =<< fromLockCache file
  where
	go lockhandle = do
		liftIO $ dropLock lockhandle
		changeLockCache $ M.delete file

getLockCache :: Annex LockCache
getLockCache = getState lockcache

fromLockCache :: FilePath -> Annex (Maybe LockHandle)
fromLockCache file = M.lookup file <$> getLockCache

changeLockCache :: (LockCache -> LockCache) -> Annex ()
changeLockCache a = do
	m <- getLockCache
	changeState $ \s -> s { lockcache = a m }

{- Runs an action with an exclusive lock held. If the lock is already
 - held, blocks until it becomes free. -}
withExclusiveLock :: (Git.Repo -> FilePath) -> Annex a -> Annex a
withExclusiveLock getlockfile a = do
	lockfile <- fromRepo getlockfile
	createAnnexDirectory $ takeDirectory lockfile
	mode <- annexFileMode
	bracket (lock mode lockfile) (liftIO . dropLock) (const a)
  where
#ifndef mingw32_HOST_OS
	lock mode = noUmask mode . lockExclusive (Just mode)
#else
	lock _mode = liftIO . waitToLock . lockExclusive
#endif

{- Tries to take an exclusive lock and run an action. If the lock is
 - already held, returns Nothing. -}
tryExclusiveLock :: (Git.Repo -> FilePath) -> Annex a -> Annex (Maybe a)
tryExclusiveLock getlockfile a = do
	lockfile <- fromRepo getlockfile
	createAnnexDirectory $ takeDirectory lockfile
	mode <- annexFileMode
	bracket (lock mode lockfile) (liftIO . unlock) go
  where
#ifndef mingw32_HOST_OS
	lock mode = noUmask mode . tryLockExclusive (Just mode)
#else
	lock _mode = liftIO . lockExclusive
#endif
	unlock = maybe noop dropLock
	go Nothing = return Nothing
	go (Just _) = Just <$> a
