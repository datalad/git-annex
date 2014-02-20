{- git-annex lock pool
 -
 - Copyright 2012, 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.LockPool where

import Common.Annex
import Annex
import Types.LockPool

import qualified Data.Map as M

#ifndef mingw32_HOST_OS
import Annex.Perms
#else
import Utility.WinLock
#endif

{- Create a specified lock file, and takes a shared lock. -}
lockFile :: FilePath -> Annex ()
lockFile file = go =<< fromPool file
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
		changePool $ M.insert file lockhandle

unlockFile :: FilePath -> Annex ()
unlockFile file = maybe noop go =<< fromPool file
  where
	go lockhandle = do
#ifndef mingw32_HOST_OS
		liftIO $ closeFd lockhandle
#else
		liftIO $ dropLock lockhandle
#endif
		changePool $ M.delete file

getPool :: Annex LockPool
getPool = getState lockpool

fromPool :: FilePath -> Annex (Maybe LockHandle)
fromPool file = M.lookup file <$> getPool

changePool :: (LockPool -> LockPool) -> Annex ()
changePool a = do
	m <- getPool
	changeState $ \s -> s { lockpool = a m }
