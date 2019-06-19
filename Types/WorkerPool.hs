{- Worker thread pool.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.WorkerPool where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Set as S

-- | Pool of worker threads. 
data WorkerPool t = WorkerPool UsedStages [Worker t]
	deriving (Show)

-- | A worker can either be idle or running an Async action.
-- And it is used for some stage.
data Worker t
	= IdleWorker t WorkerStage
	| ActiveWorker (Async t) WorkerStage

instance Show (Worker t) where
	show (IdleWorker _ s) = "IdleWorker " ++ show s
	show (ActiveWorker _ s) = "ActiveWorker " ++ show s

data WorkerStage
	= PerformStage
	-- ^ Running a CommandPerform action.
	| CleanupStage
	-- ^ Running a CommandCleanup action.
	| TransferStage
	-- ^ Transferring content to or from a remote.
	| VerifyStage
	-- ^ Verifying content, eg by calculating a checksum.
	deriving (Show, Eq, Ord)

-- | Set of stages that make sense to be used while performing an action,
-- and the stage to use initially.
-- 
-- Transitions between these stages will block as needed until there's a
-- free Worker in the pool for the new stage.
-- 
-- Actions that indicate they are in some other stage won't change the
-- stage, and so there will be no blocking before starting them.
data UsedStages = UsedStages
	{ initialStage :: WorkerStage
	, usedStages :: S.Set WorkerStage
	}
	deriving (Show)

memberStage :: WorkerStage -> UsedStages -> Bool
memberStage s u = S.member s (usedStages u)

-- | The default is to use only the CommandPerform and CommandCleanup
-- stages. Since cleanup actions often don't contend much with
-- perform actions, this prevents blocking starting the next perform action
-- on finishing the previous cleanup action.
commandStages :: UsedStages
commandStages = UsedStages
	{ initialStage = PerformStage
	, usedStages = S.fromList [PerformStage, CleanupStage]
	}

-- | When a command is transferring content, it can use this instead.
-- Transfers are often bottlenecked on the network another disk than the one
-- containing the repository, while verification bottlenecks on
-- the disk containing the repository or on the CPU.
transferStages :: UsedStages
transferStages = UsedStages
	{ initialStage = TransferStage
	, usedStages = S.fromList [TransferStage, VerifyStage]
	}

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
allocateWorkerPool :: t -> Int -> UsedStages -> WorkerPool t
allocateWorkerPool t n u = WorkerPool u $ take (n+n) $
	map (uncurry IdleWorker) $ zip (repeat t) stages
  where
	stages = concat $ repeat $ S.toList $ usedStages u

addWorkerPool :: Worker t -> WorkerPool t -> WorkerPool t
addWorkerPool w (WorkerPool u l) = WorkerPool u (w:l)

idleWorkers :: WorkerPool t -> [t]
idleWorkers (WorkerPool _ l) = go l
  where
	go [] = []
	go (IdleWorker t _ : rest) = t : go rest
	go (ActiveWorker _ _ : rest) = go rest

-- | Removes a worker from the pool whose Async uses the ThreadId.
--
-- Each Async has its own ThreadId, so this stops once it finds
-- a match.
removeThreadIdWorkerPool :: ThreadId -> WorkerPool t -> Maybe ((Async t, WorkerStage), WorkerPool t)
removeThreadIdWorkerPool tid (WorkerPool u l) = go [] l
  where
	go _ [] = Nothing
	go c (ActiveWorker a stage : rest)
		| asyncThreadId a == tid = Just ((a, stage), WorkerPool u (c++rest))
	go c (v : rest) = go (v:c) rest

deactivateWorker :: WorkerPool t -> Async t -> t -> WorkerPool t
deactivateWorker (WorkerPool u l) aid t = WorkerPool u $ go l
  where
	go [] = []
	go (w@(IdleWorker _ _) : rest) = w : go rest
	go (w@(ActiveWorker a st) : rest)
		| a == aid = IdleWorker t st : rest
		| otherwise = w : go rest

