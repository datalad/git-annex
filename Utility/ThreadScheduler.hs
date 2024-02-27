{- thread scheduling
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.ThreadScheduler (
	Seconds(..),
	Microseconds,
	runEvery,
	threadDelaySeconds,
	waitForTermination,
	oneSecond,
	unboundDelay,
) where

import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.Thread.Delay as Unbounded
#ifndef mingw32_HOST_OS
import Control.Monad.IfElse
import System.Posix.IO
#endif
#ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.Posix.Terminal
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

{- Like threadDelay, but not bounded by an Int. -}
unboundDelay :: Microseconds -> IO ()
unboundDelay = Unbounded.delay

{- Pauses the main thread, letting children run until program termination. -}
waitForTermination :: IO ()
waitForTermination = do
#ifdef mingw32_HOST_OS
	forever $ threadDelaySeconds (Seconds 6000)
#else
	lock <- newEmptyMVar
	let check sig = void $
		installHandler sig (CatchOnce $ putMVar lock ()) Nothing
	check softwareTermination
	whenM (queryTerminal stdInput) $
		check keyboardSignal
	takeMVar lock
#endif

oneSecond :: Microseconds
oneSecond = 1000000
