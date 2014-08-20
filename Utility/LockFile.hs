{- Lock files
 -
 - Posix and Windows lock files are extremely different.
 - This module does *not* attempt to be a portability shim, it just exposes
 - the native locking of the OS.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
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
