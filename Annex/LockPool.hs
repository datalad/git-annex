{- Wraps Utility.LockPool, making pid locks be used when git-annex is so
 - configured.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.LockPool (module X) where

#ifndef mingw32_HOST_OS
import Annex.LockPool.PosixOrPid as X
#else
import Utility.LockPool.Windows as X
#endif
