{- git-annex lock files.
 -
 - Copyright 2012, 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.LockFile (
	lockFileShared,
	unlockFile,
	getLockPool,
	withExclusiveLock,
) where

import Common.Annex
import Annex
import Types.LockPool
import qualified Git
import Annex.Perms
import Utility.LockFile

import qualified Data.Map as M

{- Create a specified lock file, and takes a shared lock, which is retained
 - in the pool. -}
lockFileShared :: FilePath -> Annex ()
lockFileShared file = go =<< fromLockPool file
  where
	go (Just _) = noop -- already locked
	go Nothing = do
#ifndef mingw32_HOST_OS
		mode <- annexFileMode
		lockhandle <- liftIO $ noUmask mode $ lockShared (Just mode) file
#else
		lockhandle <- liftIO $ waitToLock $ lockShared file
#endif
		changeLockPool $ M.insert file lockhandle

unlockFile :: FilePath -> Annex ()
unlockFile file = maybe noop go =<< fromLockPool file
  where
	go lockhandle = do
		liftIO $ dropLock lockhandle
		changeLockPool $ M.delete file

getLockPool :: Annex LockPool
getLockPool = getState lockpool

fromLockPool :: FilePath -> Annex (Maybe LockHandle)
fromLockPool file = M.lookup file <$> getLockPool

changeLockPool :: (LockPool -> LockPool) -> Annex ()
changeLockPool a = do
	m <- getLockPool
	changeState $ \s -> s { lockpool = a m }

{- Runs an action with an exclusive lock held. If the lock is already
 - held, blocks until it becomes free. -}
withExclusiveLock :: (Git.Repo -> FilePath) -> Annex a -> Annex a
withExclusiveLock getlockfile a = do
	lockfile <- fromRepo getlockfile
	createAnnexDirectory $ takeDirectory lockfile
	mode <- annexFileMode
	bracketIO (lock mode lockfile) dropLock (const a)
  where
#ifndef mingw32_HOST_OS
	lock mode = noUmask mode . lockExclusive (Just mode)
#else
	lock _mode = waitToLock . lockExclusive
#endif
