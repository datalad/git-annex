{- git-annex concurrency utilities
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Concurrent.Utility where

import Types.Concurrency

import GHC.Conc

{- Honor the requested level of concurrency, but only up to the number of
 - CPU cores. Useful for things that are known to be CPU bound. -}
concurrencyUpToCpus :: Concurrency -> IO Int
concurrencyUpToCpus c = do
	let cn = case c of
		Concurrent n -> n
		NonConcurrent -> 1
		ConcurrentPerCpu -> 1
	pn <- getNumProcessors
	return (min cn pn)
