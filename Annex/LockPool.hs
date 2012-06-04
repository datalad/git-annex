{- git-annex lock pool
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.LockPool where

import qualified Data.Map as M
import System.Posix.Types (Fd)

import Common.Annex
import Annex
import Annex.Perms

{- Create a specified lock file, and takes a shared lock. -}
lockFile :: FilePath -> Annex ()
lockFile file = go =<< fromPool file
	where
		go (Just _) = noop -- already locked
		go Nothing = do
			mode <- annexFileMode
			fd <- liftIO $ noUmask mode $
				openFd file ReadOnly (Just mode) defaultFileFlags
			liftIO $ waitToSetLock fd (ReadLock, AbsoluteSeek, 0, 0)
			changePool $ M.insert file fd

unlockFile :: FilePath -> Annex ()
unlockFile file = maybe noop go =<< fromPool file
	where
		go fd = do
			liftIO $ closeFd fd
			changePool $ M.delete file

getPool :: Annex (M.Map FilePath Fd)
getPool = getState lockpool

fromPool :: FilePath -> Annex (Maybe Fd)
fromPool file = M.lookup file <$> getPool

changePool :: (M.Map FilePath Fd -> M.Map FilePath Fd) -> Annex ()
changePool a = do
	m <- getPool
	changeState $ \s -> s { lockpool = a m }
