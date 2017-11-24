{- Lock files
 -
 - Posix and Windows lock files are extremely different.
 - This module does *not* attempt to be a portability shim, it just exposes
 - the native locking of the OS.
 -
 - Posix fcntl locks have some gotchas. So, consider using
 - Utility.LockPool instead of using this module directly.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.LockFile (module X) where

#ifndef mingw32_HOST_OS
import Utility.LockFile.Posix as X
#else
import Utility.LockFile.Windows as X
#endif
