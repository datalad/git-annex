{- Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Concurrency where

data Concurrency = NonConcurrent | Concurrent Int
