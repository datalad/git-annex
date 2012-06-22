{- thread scheduling
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 - Copyright 2011 Bas van Dijk & Roel van Dijk
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.ThreadScheduler where

import Common

import Control.Concurrent
import System.Posix.Terminal
import System.Posix.Signals

newtype Seconds = Seconds { fromSeconds :: Int }
	deriving (Eq, Ord, Show)

{- Runs an action repeatedly forever, sleeping at least the specified number
 - of seconds in between. -}
runEvery :: Seconds -> IO a -> IO a
runEvery n a = forever $ do
	threadDelaySeconds n
	a

runEveryWith :: Seconds -> a -> (a -> IO a) -> IO ()
runEveryWith n val a = do
	threadDelaySeconds n
	val' <- a val
	runEveryWith n val' a

threadDelaySeconds :: Seconds -> IO ()
threadDelaySeconds (Seconds n) = unboundDelay (fromIntegral n * oneSecond)
	where
		oneSecond = 1000000 -- microseconds

{- Like threadDelay, but not bounded by an Int.
 -
 - There is no guarantee that the thread will be rescheduled promptly when the
 - delay has expired, but the thread will never continue to run earlier than
 - specified.
 - 
 - Taken from the unbounded-delay package to avoid a dependency for 4 lines
 - of code.
 -}
unboundDelay :: Integer -> IO ()
unboundDelay time = do
	let maxWait = min time $ toInteger (maxBound :: Int)
	threadDelay $ fromInteger maxWait
	when (maxWait /= time) $ unboundDelay (time - maxWait)

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
