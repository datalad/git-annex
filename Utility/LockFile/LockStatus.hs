{- LockStatus type
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockFile.LockStatus where

import System.Posix

data LockStatus = StatusUnLocked | StatusLockedBy ProcessID | StatusNoLockFile
	deriving (Eq)
