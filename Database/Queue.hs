{- Persistent sqlite database queues
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Database.Queue (
	DbQueue,
	initDb,
	openDbQueue,
	queryDbQueue,
	closeDbQueue,
	flushDbQueue,
	QueueSize,
	queueDb,
) where

import Utility.Monad
import Database.Handle

import Database.Persist.Sqlite
import Control.Concurrent
import Data.Time.Clock
import Control.Applicative
import Prelude

{- A DbQueue wraps a DbHandle, adding a queue of writes to perform.
 -
 - This is efficient when there are frequent writes, but
 - reads will not immediately have access to queued writes. -}
data DbQueue = DQ DbHandle (MVar Queue)

{- Opens the database queue, but does not perform any migrations. Only use
 - if the database is known to exist and have the right tables; ie after
 - running initDb. -}
openDbQueue :: FilePath -> TableName -> IO DbQueue
openDbQueue db tablename = DQ
	<$> openDb db tablename
	<*> (newMVar =<< emptyQueue)

{- This or flushDbQueue must be called, eg at program exit to ensure
 - queued changes get written to the database. -}
closeDbQueue :: DbQueue -> IO ()
closeDbQueue h@(DQ hdl _) = do
	flushDbQueue h
	closeDb hdl

{- Blocks until all queued changes have been written to the database. -}
flushDbQueue :: DbQueue -> IO ()
flushDbQueue (DQ hdl qvar) = do
	q@(Queue sz _ qa) <- takeMVar qvar
	if sz > 0
		then do
			commitDb hdl qa
			putMVar qvar =<< emptyQueue
		else putMVar qvar q

{- Makes a query using the DbQueue's database connection.
 - This should not be used to make changes to the database!
 -
 - Queries will not return changes that have been recently queued,
 - so use with care.
 -}
queryDbQueue :: DbQueue -> SqlPersistM a -> IO a
queryDbQueue (DQ hdl _) = queryDb hdl

{- A queue of actions to perform, with a count of the number of actions
 - queued, and a last commit time. -}
data Queue = Queue QueueSize LastCommitTime (SqlPersistM ())

type QueueSize = Int

type LastCommitTime = UTCTime

emptyQueue :: IO Queue
emptyQueue = do
	now <- getCurrentTime
	return $ Queue 0 now (return ())

{- Queues a change to be made to the database. It will be queued
 - to be committed later, unless the commitchecker action returns true,
 - in which case any previously queued changes are also committed.
 -
 - Transactions built up by queueDb are sent to sqlite all at once.
 - If sqlite fails due to another change being made concurrently by another
 - process, the transaction is put back in the queue. This avoids
 - the sqlite multiple writer problem.
 -}
queueDb 
	:: DbQueue
	-> (QueueSize -> LastCommitTime -> IO Bool) 
	-> SqlPersistM ()
	-> IO ()
queueDb (DQ hdl qvar) commitchecker a = do
	Queue sz lastcommittime qa <- takeMVar qvar
	let !sz' = sz + 1
	let qa' = qa >> a
	let enqueue = putMVar qvar
	ifM (commitchecker sz' lastcommittime)
		( do
			r <- commitDb' hdl qa'
			case r of
				Left _ -> enqueue $ Queue sz' lastcommittime qa'
				Right _ -> enqueue =<< emptyQueue
		, enqueue $ Queue sz' lastcommittime qa'
		)
