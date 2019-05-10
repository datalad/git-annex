{- Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Concurrency where

import Utility.PartialPrelude

data Concurrency = NonConcurrent | Concurrent Int | ConcurrentPerCpu

parseConcurrency :: String -> Maybe Concurrency
parseConcurrency "cpus" = Just ConcurrentPerCpu
parseConcurrency "cpu" = Just ConcurrentPerCpu
parseConcurrency s = Concurrent <$> readish s
