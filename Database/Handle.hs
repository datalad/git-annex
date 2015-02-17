{- Persistent sqlite database handles.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Database.Handle (
	DbHandle,
	openDb,
	runDb,
	commitDb,
	closeDb,
	Size,
	queueDb,
	flushQueueDb,
) where

import Utility.Exception
import Messages

import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throwIO)
import qualified Data.Text as T

{- A DbHandle is a reference to a worker thread that communicates with
 - the database. It has a MVar which Jobs are submitted to. -}
data DbHandle = DbHandle (Async ()) (MVar Job) (MVar DbQueue)

data Job = RunJob (SqlPersistM ()) | CommitJob | CloseJob

openDb :: FilePath -> IO DbHandle
openDb db = do
	jobs <- newEmptyMVar
	worker <- async (workerThread (T.pack db) jobs)
	q <- newMVar emptyDbQueue
	return $ DbHandle worker jobs q

workerThread :: T.Text -> MVar Job -> IO ()
workerThread db jobs = catchNonAsync go showerr
  where
  	showerr e = liftIO $ warningIO $ "sqlite worker thread crashed: " ++ show e
	go = do
		r <- runSqlite db transaction
		case r of
			CloseJob -> return ()
			_ -> go
	transaction = do
		job <- liftIO $ takeMVar jobs
		case job of
			RunJob a -> a >> transaction
			CommitJob -> return CommitJob
			CloseJob -> return CloseJob

{- Runs an action using the DbHandle. The action may be a query, or it may
 - make a change. Changes are bundled up in a transaction, which does not
 - complete until commitDb is called.
 -
 - Note that the action is not run by the calling thread, but by a
 - worker thread. Exceptions are propigated to the calling thread.
 -
 - Only one action can be run at a time against a given DbHandle.
 - If called concurrently in the same process, this will block until
 - it is able to run.
 -
 - Note that if multiple processes are trying to change the database
 - at the same time, sqlite will only let one build a transaction at a
 - time.
 -}
runDb :: DbHandle -> SqlPersistM a -> IO a
runDb (DbHandle _ jobs _) a = do
	res <- newEmptyMVar
	putMVar jobs $ RunJob $
		liftIO . putMVar res =<< tryNonAsync a
	either throwIO return =<< takeMVar res

{- Commits any transaction that was created by the previous calls to runDb,
 - and starts a new transaction. -}
commitDb :: DbHandle -> IO ()
commitDb (DbHandle _ jobs _) = putMVar jobs CommitJob

closeDb :: DbHandle -> IO ()
closeDb h@(DbHandle worker jobs _) = do
	flushQueueDb h
	putMVar jobs CloseJob
	wait worker

type Size = Int

{- A queue of actions to perform, with a count of the number of actions
 - queued. -}
data DbQueue = DbQueue Size (SqlPersistM ())

emptyDbQueue :: DbQueue
emptyDbQueue = DbQueue 0 (return ())

{- Queues a change to be committed to the database. It will be buffered
 - to be committed later, unless the queue gets larger than the specified
 - size.
 -
 - (Be sure to call closeDb or flushQueue to ensure the change gets committed.)
 -
 - Transactions built up by queueDb are sent to sqlite all at once.
 - If sqlite fails due to another change being made concurrently by another
 - process, the transaction is put back in the queue. This solves
 - the sqlite multiple writer problem.
 -}
queueDb :: DbHandle -> Size -> SqlPersistM () -> IO ()
queueDb h@(DbHandle _ _ qvar) maxsz a = do
	DbQueue sz qa <- takeMVar qvar
	let !sz' = sz + 1
	let qa' = qa >> a
	let enqueue = putMVar qvar (DbQueue sz' qa')
	if sz' > maxsz
		then do
			r <- tryNonAsync $ do
				runDb h qa'
				commitDb h
			case r of
				Left _ -> enqueue
				Right _ -> putMVar qvar emptyDbQueue
		else enqueue

{- If flushing the queue fails, this could be because there is another
 - writer to the database. Retry repeatedly for up to 10 seconds. -}
flushQueueDb :: DbHandle -> IO ()
flushQueueDb h@(DbHandle _ _ qvar) = do
	DbQueue sz qa <- takeMVar qvar	
	when (sz > 0) $
		robustly 100 $ runDb h qa
  where
	robustly :: Int -> IO () -> IO ()
	robustly 0 _ = error "failed to commit changes to sqlite database"
	robustly n a = catchNonAsync a $ \_ -> do
		threadDelay 100000 -- 1/10th second
		robustly (n-1) a
