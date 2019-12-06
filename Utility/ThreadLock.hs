{- locking between threads
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.ThreadLock (
	Lock,
	newLock,
	withLock,
) where

import Control.Concurrent.MVar

type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

{- Runs an action with a lock held, so only one thread at a time can run it. -}
withLock :: Lock -> IO a -> IO a
withLock lock = withMVar lock . const
