{- Monotonic clocks
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Utility.MonotonicClock where

import qualified System.Clock as Clock
#ifdef linux_HOST_OS
import Utility.Exception
#endif

newtype MonotonicTimestamp = MonotonicTimestamp Integer
	deriving (Show, Eq, Ord, Num)

-- On linux, this uses a clock that advances while the system is suspended,
-- except for on very old kernels (eg 2.6.32).
-- On other systems, that is not available, and the monotonic clock will
-- not advance while suspended.
currentMonotonicTimestamp :: IO MonotonicTimestamp
currentMonotonicTimestamp =
	(MonotonicTimestamp . fromIntegral . Clock.sec) <$>
#ifdef linux_HOST_OS
		(tryNonAsync (Clock.getTime Clock.Boottime)
			>>= either (const $ Clock.getTime Clock.Monotonic) return)
#else
		Clock.getTime Clock.Monotonic
#endif
