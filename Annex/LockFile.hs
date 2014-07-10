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
import Annex.Exception

import qualified Data.Map as M

#ifndef mingw32_HOST_OS
import Annex.Perms
#else
import Utility.WinLock
#endif

{- Create a specified lock file, and takes a shared lock, which is retained
 - in the pool. -}
lockFileShared :: FilePath -> Annex ()
lockFileShared file = go =<< fromLockPool file
  where
	go (Just _) = noop -- already locked
	go Nothing = do
#ifndef mingw32_HOST_OS
		mode <- annexFileMode
		lockhandle <- liftIO $ noUmask mode $
			openFd file ReadOnly (Just mode) defaultFileFlags
		liftIO $ waitToSetLock lockhandle (ReadLock, AbsoluteSeek, 0, 0)
#else
		lockhandle <- liftIO $ waitToLock $ lockShared file
#endif
		changeLockPool $ M.insert file lockhandle

unlockFile :: FilePath -> Annex ()
unlockFile file = maybe noop go =<< fromLockPool file
  where
	go lockhandle = do
#ifndef mingw32_HOST_OS
		liftIO $ closeFd lockhandle
#else
		liftIO $ dropLock lockhandle
#endif
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
	bracketIO (lock lockfile mode) unlock (const a)
  where
#ifndef mingw32_HOST_OS
	lock lockfile mode = do
		l <- noUmask mode $ createFile lockfile mode
		waitToSetLock l (WriteLock, AbsoluteSeek, 0, 0)
		return l
	unlock = closeFd
#else
	lock lockfile _mode = waitToLock $ lockExclusive lockfile
	unlock = dropLock
#endif
