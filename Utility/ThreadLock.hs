{- locking between threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.ThreadLock where

import Control.Concurrent.MVar

type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

{- Runs an action with a lock held, so only one thread at a time can run it. -}
withLock :: Lock -> IO a -> IO a
withLock lock = withMVar lock . const
