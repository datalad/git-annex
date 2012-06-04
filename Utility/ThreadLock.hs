{- locking between threads
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.ThreadLock where

import Common

import System.Posix.Terminal
import Control.Concurrent.MVar
import System.Posix.Signals

type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

{- Runs an action with a lock held, so only one thread at a time can run it. -}
withLock :: Lock -> IO a -> IO a
withLock lock = withMVar lock . const

{- Pauses the main thread, letting children run until program termination. -}
waitForTermination :: IO ()
waitForTermination = do
	lock <- newEmptyMVar
	check softwareTermination lock
	whenM (queryTerminal stdInput) $
		check keyboardSignal lock
	takeMVar lock
	where
		check sig lock = void $
			installHandler sig (CatchOnce $ putMVar lock ()) Nothing
