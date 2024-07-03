{- Monotonic clocks
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.MonotonicClock where

#if MIN_VERSION_clock(0,3,0)
import qualified System.Clock as Clock
#else
import qualified System.Posix.Clock as Clock
#endif

newtype MonotonicTimestamp = MonotonicTimestamp Integer
	deriving (Show, Eq, Ord)

currentMonotonicTimestamp :: IO MonotonicTimestamp
currentMonotonicTimestamp =
	(MonotonicTimestamp . fromIntegral . Clock.sec)
		<$> Clock.getTime Clock.Monotonic
