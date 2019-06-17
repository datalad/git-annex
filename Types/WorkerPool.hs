{- Command worker pool.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.WorkerPool where

import Control.Concurrent
import Control.Concurrent.Async

-- | Pool of worker threads. 
data WorkerPool t
	= UnallocatedWorkerPool
	| WorkerPool [Worker t]
	deriving (Show)

-- | A worker can either be idle or running an Async action.
-- And it is used for some stage.
data Worker t
	= IdleWorker t WorkerStage
	| ActiveWorker (Async t) WorkerStage

instance Show (Worker t) where
	show (IdleWorker _ s) = "IdleWorker " ++ show s
	show (ActiveWorker _ s) = "ActiveWorker " ++ show s

-- | These correspond to CommandPerform and CommandCleanup.
data WorkerStage = PerformStage | CleanupStage
	deriving (Show, Eq)

workerStage :: Worker t -> WorkerStage
workerStage (IdleWorker _ s) = s
workerStage (ActiveWorker _ s) = s

workerAsync :: Worker t -> Maybe (Async t)
workerAsync (IdleWorker _ _) = Nothing
workerAsync (ActiveWorker aid _) = Just aid

-- | Allocates a WorkerPool that has the specified number of workers
-- in it, of each stage.
--
-- The stages are distributed evenly throughout.
allocateWorkerPool :: t -> Int -> WorkerPool t
allocateWorkerPool t n = WorkerPool $ take (n+n) $
	map (uncurry IdleWorker) $ zip (repeat t) stages
  where
	stages = concat $ repeat [PerformStage, CleanupStage]

addWorkerPool :: Worker t -> WorkerPool t -> WorkerPool t
addWorkerPool w (WorkerPool l) = WorkerPool (w:l)
addWorkerPool w UnallocatedWorkerPool = WorkerPool [w]

idleWorkers :: WorkerPool t -> [t]
idleWorkers UnallocatedWorkerPool = []
idleWorkers (WorkerPool l) = go l
  where
	go [] = []
	go (IdleWorker t _ : rest) = t : go rest
	go (ActiveWorker _ _ : rest) = go rest

-- | Removes a worker from the pool whose Async uses the ThreadId.
--
-- Each Async has its own ThreadId, so this stops once it finds
-- a match.
removeThreadIdWorkerPool :: ThreadId -> WorkerPool t -> Maybe ((Async t, WorkerStage), WorkerPool t)
removeThreadIdWorkerPool _ UnallocatedWorkerPool = Nothing
removeThreadIdWorkerPool tid (WorkerPool l) = go [] l
  where
	go _ [] = Nothing
	go c (ActiveWorker a stage : rest)
		| asyncThreadId a == tid = Just ((a, stage), WorkerPool (c++rest))
	go c (v : rest) = go (v:c) rest

deactivateWorker :: WorkerPool t -> Async t -> t -> WorkerPool t
deactivateWorker UnallocatedWorkerPool _ _ = UnallocatedWorkerPool
deactivateWorker (WorkerPool l) aid t = WorkerPool $ go l
  where
	go [] = []
	go (w@(IdleWorker _ _) : rest) = w : go rest
	go (w@(ActiveWorker a st) : rest)
		| a == aid = IdleWorker t st : rest
		| otherwise = w : go rest

