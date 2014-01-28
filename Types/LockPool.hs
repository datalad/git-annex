{- git-annex lock pool data types
 - 
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.LockPool (
	LockPool,
	LockHandle
) where

import qualified Data.Map as M

#ifndef mingw32_HOST_OS
import System.Posix.Types (Fd)
type LockHandle = Fd
#else
import Utility.WinLock -- defines LockHandle
#endif

type LockPool = M.Map FilePath LockHandle
