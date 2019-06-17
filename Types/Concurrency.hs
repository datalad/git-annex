{- Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Concurrency where

import Utility.PartialPrelude

-- Note that Concurrent 1 is not the same as NonConcurrent;
-- the former specifies 1 job of each particular kind, but there can be
-- more than one kind of job running concurrently.
data Concurrency = NonConcurrent | Concurrent Int | ConcurrentPerCpu

parseConcurrency :: String -> Maybe Concurrency
parseConcurrency "cpus" = Just ConcurrentPerCpu
parseConcurrency "cpu" = Just ConcurrentPerCpu
parseConcurrency s = Concurrent <$> readish s
