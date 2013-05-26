{- thread scheduling
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 - Copyright 2011 Bas van Dijk & Roel van Dijk
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.ThreadScheduler where

import Common

import Control.Concurrent
import System.Posix.Signals
#ifndef __ANDROID__
#ifndef __WINDOWS__
import System.Posix.Terminal
#else
#endif

newtype Seconds = Seconds { fromSeconds :: Int }
	deriving (Eq, Ord, Show)

type Microseconds = Integer

{- Runs an action repeatedly forever, sleeping at least the specified number
 - of seconds in between. -}
runEvery :: Seconds -> IO a -> IO a
runEvery n a = forever $ do
	threadDelaySeconds n
	a

threadDelaySeconds :: Seconds -> IO ()
threadDelaySeconds (Seconds n) = unboundDelay (fromIntegral n * oneSecond)

{- Like threadDelay, but not bounded by an Int.
 -
 - There is no guarantee that the thread will be rescheduled promptly when the
 - delay has expired, but the thread will never continue to run earlier than
 - specified.
 - 
 - Taken from the unbounded-delay package to avoid a dependency for 4 lines
 - of code.
 -}
unboundDelay :: Microseconds -> IO ()
unboundDelay time = do
	let maxWait = min time $ toInteger (maxBound :: Int)
	threadDelay $ fromInteger maxWait
	when (maxWait /= time) $ unboundDelay (time - maxWait)

{- Pauses the main thread, letting children run until program termination. -}
waitForTermination :: IO ()
waitForTermination = do
	lock <- newEmptyMVar
	check softwareTermination lock
#ifndef __ANDROID__
#ifndef __WINDOWS__
	whenM (queryTerminal stdInput) $
		check keyboardSignal lock
#endif
#endif
	takeMVar lock
  where
	check sig lock = void $
		installHandler sig (CatchOnce $ putMVar lock ()) Nothing

oneSecond :: Microseconds
oneSecond = 1000000
