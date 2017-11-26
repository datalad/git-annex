{- Lock pool.
 -
 - This avoids a problem with unix fcntl locks: They are not composition-safe.
 -
 - For example, if one thread is holding a lock, and another thread opens the
 - lock file (to attempt to take or check the lock), and then closes it,
 - the lock will be released, despite the first thread still having the
 - lockfile open.
 -
 - Or, if a process is already holding an exclusive lock on a file, and
 - re-opens it and tries to take another exclusive lock, it won't block
 - on the first lock.
 -
 - To avoid these problems, this implements a lock pool. This keeps track
 - of which lock files are being used by the process, using STM to handle
 - inter-process locking.
 -
 - Note that, like Utility.LockFile, this does *not* attempt to be a
 - portability shim; the native locking of the OS is used.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.LockPool (module X) where

#ifndef mingw32_HOST_OS
import Utility.LockPool.Posix as X
#else
import Utility.LockPool.Windows as X
#endif
