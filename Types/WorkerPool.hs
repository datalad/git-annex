{- Worker thread pool.
 -
 - Copyright 2019-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.WorkerPool where

import Types.Direction

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Set as S

-- | Pool of worker threads. 
data WorkerPool t = WorkerPool
	{ usedStages :: UsedStages
	, workerList :: [Worker t]
	, spareVals :: [t]
	-- ^ Normally there is one value for each IdleWorker,
	-- but there can temporarily be fewer values, when a thread is
	-- changing between stages.
	} 

instance Show (WorkerPool t) where
	show p = unwords
		[ "WorkerPool"
		, show (usedStages p)
		, show (workerList p)
		, show (length (spareVals p))
		]

-- | A worker can either be idle or running an Async action.
-- And it is used for some stage.
data Worker t
	= IdleWorker WorkerStage
	| ActiveWorker (Async t) WorkerStage

instance Show (Worker t) where
	show (IdleWorker s) = "IdleWorker " ++ show s
	show (ActiveWorker _ s) = "ActiveWorker " ++ show s

data WorkerStage
	= StartStage
	-- ^ All threads start in this stage, and then transition away from
	-- it to the initialStage when they begin doing work. This should
	-- never be included in UsedStages, because transition from some
	-- other stage back to this one could result in a deadlock.
	| PerformStage
	-- ^ Running a CommandPerform action.
	| CleanupStage
	-- ^ Running a CommandCleanup action.
	| TransferStage Direction
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
	, stageSet :: S.Set WorkerStage
	}
	deriving (Show)

memberStage :: WorkerStage -> UsedStages -> Bool
memberStage s u = S.member s (stageSet u)

-- | The default is to use only the CommandPerform and CommandCleanup
-- stages. Since cleanup actions often don't contend much with
-- perform actions, this prevents blocking starting the next perform action
-- on finishing the previous cleanup action.
commandStages :: UsedStages
commandStages = UsedStages
	{ initialStage = PerformStage
	, stageSet = S.fromList [PerformStage, CleanupStage]
	}

-- | This is mostly useful for downloads, not for uploads. A download
-- is often bottlenecked on the network or another disk than the one
-- containing the repository. When verification is not done incrementally,
-- it bottlenecks on the disk containing the repository or on the CPU.
-- So it makes sense to run the download and verify stages separately.
-- 
-- For uploads, there is no separate verify step to this is less likely
-- to be useful than commandStages. However, a separate stage is provided
-- for Uploads. That can be useful when a command downloads from one remote
-- (eg using the network) and uploads to another remote (eg using a disk).
transferStages :: UsedStages
transferStages = UsedStages
	{ initialStage = TransferStage Download
	, stageSet = S.fromList
		[ TransferStage Download
		, TransferStage Upload
		, VerifyStage
		]
	}

workerStage :: Worker t -> WorkerStage
workerStage (IdleWorker s) = s
workerStage (ActiveWorker _ s) = s

workerAsync :: Worker t -> Maybe (Async t)
workerAsync (IdleWorker _) = Nothing
workerAsync (ActiveWorker aid _) = Just aid

-- | Allocates a WorkerPool that has the specified number of workers
-- in it, of each stage.
--
-- The stages are distributed evenly throughout.
allocateWorkerPool :: t -> Int -> UsedStages -> WorkerPool t
allocateWorkerPool t n u = WorkerPool
	{ usedStages = u
	, workerList = map IdleWorker $
		take totalthreads $ concat $ repeat stages
	, spareVals = replicate totalthreads t
	}
  where
	stages = StartStage : S.toList (stageSet u)
	totalthreads = n * length stages

addWorkerPool :: Worker t -> WorkerPool t -> WorkerPool t
addWorkerPool w pool = pool { workerList = w : workerList pool }

-- | Removes a worker from the pool whose Async uses the ThreadId.
--
-- Each Async has its own ThreadId, so this stops once it finds
-- a match.
removeThreadIdWorkerPool :: ThreadId -> WorkerPool t -> Maybe ((Async t, WorkerStage), WorkerPool t)
removeThreadIdWorkerPool tid pool = go [] (workerList pool)
  where
	go _ [] = Nothing
	go c (ActiveWorker a stage : rest)
		| asyncThreadId a == tid =
			let pool' = pool { workerList = (c++rest) }
			in Just ((a, stage), pool')
	go c (v : rest) = go (v:c) rest

deactivateWorker :: WorkerPool t -> Async t -> t -> WorkerPool t
deactivateWorker pool aid t = pool
	{ workerList = go (workerList pool)
	, spareVals = t : spareVals pool
	}
  where
	go [] = []
	go (w@(IdleWorker _) : rest) = w : go rest
	go (w@(ActiveWorker a st) : rest)
		| a == aid = IdleWorker st : rest
		| otherwise = w : go rest

allIdle :: WorkerPool t -> Bool
allIdle pool = all idle (workerList pool)
	-- If this does not hold, a thread must be transitioning between
	-- states, so it's not really idle.
	&& length (spareVals pool) == length (workerList pool)
  where
	idle (IdleWorker _) = True
	idle (ActiveWorker _ _) = False
