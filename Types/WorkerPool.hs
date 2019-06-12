{- Command worker pool.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.WorkerPool where

import Control.Concurrent.Async
import Data.Either

-- | Pool of worker threads. 
data WorkerPool t
	= UnallocatedWorkerPool
	| WorkerPool [Worker t]

-- | A worker can either be idle or running an Async action.
type Worker t = Either t (Async t)

allocateWorkerPool :: t -> Int -> WorkerPool t
allocateWorkerPool t n = WorkerPool $ replicate n (Left t)

addWorkerPool :: WorkerPool t -> Worker t -> WorkerPool t
addWorkerPool (WorkerPool l) w = WorkerPool (w:l)
addWorkerPool UnallocatedWorkerPool w = WorkerPool [w]

idleWorkers :: WorkerPool t -> [t]
idleWorkers UnallocatedWorkerPool = []
idleWorkers (WorkerPool l) = lefts l
