{- Persistent sqlite database handles.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Database.Handle (
	DbHandle,
	initDb,
	openDb,
	queryDb,
	closeDb,
	Size,
	queueDb,
	flushQueueDb,
	commitDb,
) where

import Utility.Exception
import Utility.Monad
import Messages

import Database.Persist.Sqlite
import qualified Database.Sqlite as Sqlite
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throwIO)
import qualified Data.Text as T
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import Data.List
import Data.Time.Clock

{- A DbHandle is a reference to a worker thread that communicates with
 - the database. It has a MVar which Jobs are submitted to. -}
data DbHandle = DbHandle (Async ()) (MVar Job) (MVar DbQueue)

{- Ensures that the database is initialized. Pass the migration action for
 - the database.
 -
 - The database is put into WAL mode, to prevent readers from blocking
 - writers, and prevent a writer from blocking readers.
 -}
initDb :: FilePath -> SqlPersistM () -> IO ()
initDb f migration = do
	let db = T.pack f
	enableWAL db
	runSqlite db migration

enableWAL :: T.Text -> IO ()
enableWAL db = do
	conn <- Sqlite.open db
	stmt <- Sqlite.prepare conn (T.pack "PRAGMA journal_mode=WAL;")
	void $ Sqlite.step stmt
	void $ Sqlite.finalize stmt
	Sqlite.close conn

{- Opens the database, but does not perform any migrations. Only use
 - if the database is known to exist and have the right tables. -}
openDb :: FilePath -> TableName -> IO DbHandle
openDb db tablename = do
	jobs <- newEmptyMVar
	worker <- async (workerThread (T.pack db) tablename jobs)
	q <- newMVar =<< emptyDbQueue
	return $ DbHandle worker jobs q

data Job
	= QueryJob (SqlPersistM ())
	| ChangeJob ((SqlPersistM () -> IO ()) -> IO ())
	| CloseJob

type TableName = String

workerThread :: T.Text -> TableName -> MVar Job -> IO ()
workerThread db tablename jobs = catchNonAsync (run loop) showerr
  where
  	showerr e = liftIO $ warningIO $
		"sqlite worker thread crashed: " ++ show e
	
	loop = do
		job <- liftIO $ takeMVar jobs
		case job of
			QueryJob a -> a >> loop
			-- change is run in a separate database connection
			-- since sqlite only supports a single writer at a
			-- time, and it may crash the database connection
			ChangeJob a -> liftIO (a run) >> loop
			CloseJob -> return ()
	
	-- like runSqlite, but calls settle on the raw sql Connection.
	run a = do
		conn <- Sqlite.open db
		settle conn
		runResourceT $ runNoLoggingT $
			withSqlConn (wrapConnection conn) $
				runSqlConn a

	-- Work around a bug in sqlite: New database connections can
	-- sometimes take a while to become usable; select statements will
	-- fail with ErrorBusy for some time. So, loop until a select
	-- succeeds; once one succeeds the connection will stay usable.
	-- <http://thread.gmane.org/gmane.comp.db.sqlite.general/93116>
	settle conn = do
		r <- tryNonAsync $ do
			stmt <- Sqlite.prepare conn nullselect
			void $ Sqlite.step stmt
			void $ Sqlite.finalize stmt
		case r of
			Right _ -> return ()
			Left e -> do
				if "ErrorBusy" `isInfixOf` show e
					then do
						threadDelay 1000 -- 1/1000th second
						settle conn
					else throwIO e
	
	-- This should succeed for any table.
	nullselect = T.pack $ "SELECT null from " ++ tablename ++ " limit 1"

{- Makes a query using the DbHandle. This should not be used to make
 - changes to the database!
 -
 - Note that the action is not run by the calling thread, but by a
 - worker thread. Exceptions are propigated to the calling thread.
 -
 - Only one action can be run at a time against a given DbHandle.
 - If called concurrently in the same process, this will block until
 - it is able to run.
 -}
queryDb :: DbHandle -> SqlPersistM a -> IO a
queryDb (DbHandle _ jobs _) a = do
	res <- newEmptyMVar
	putMVar jobs $ QueryJob $
		liftIO . putMVar res =<< tryNonAsync a
	(either throwIO return =<< takeMVar res)
		`catchNonAsync` (const $ error "sqlite query crashed")

closeDb :: DbHandle -> IO ()
closeDb h@(DbHandle worker jobs _) = do
	flushQueueDb h
	putMVar jobs CloseJob
	wait worker

type Size = Int

type LastCommitTime = UTCTime

{- A queue of actions to perform, with a count of the number of actions
 - queued, and a last commit time. -}
data DbQueue = DbQueue Size LastCommitTime (SqlPersistM ())

emptyDbQueue :: IO DbQueue
emptyDbQueue = do
	now <- getCurrentTime
	return $ DbQueue 0 now (return ())

{- Queues a change to be made to the database. It will be buffered
 - to be committed later, unless the commitchecker action returns true.
 -
 - (Be sure to call closeDb or flushQueueDb to ensure the change
 - gets committed.)
 -
 - Transactions built up by queueDb are sent to sqlite all at once.
 - If sqlite fails due to another change being made concurrently by another
 - process, the transaction is put back in the queue. This solves
 - the sqlite multiple writer problem.
 -}
queueDb 
	:: DbHandle
	-> (Size -> LastCommitTime -> IO Bool) 
	-> SqlPersistM ()
	-> IO ()
queueDb h@(DbHandle _ _ qvar) commitchecker a = do
	DbQueue sz lastcommittime qa <- takeMVar qvar
	let !sz' = sz + 1
	let qa' = qa >> a
	let enqueue = putMVar qvar
	ifM (commitchecker sz' lastcommittime)
		( do
			r <- commitDb h qa'
			case r of
				Left _ -> enqueue $ DbQueue sz' lastcommittime qa'
				Right _ -> do
					now <- getCurrentTime
					enqueue $ DbQueue 0 now (return ())
		, enqueue $ DbQueue sz' lastcommittime qa'
		)

{- If flushing the queue fails, this could be because there is another
 - writer to the database. Retry repeatedly for up to 10 seconds. -}
flushQueueDb :: DbHandle -> IO ()
flushQueueDb h@(DbHandle _ _ qvar) = do
	DbQueue sz _ qa <- takeMVar qvar	
	when (sz > 0) $
		robustly Nothing 100 (commitDb h qa)
  where
	robustly :: Maybe SomeException -> Int -> IO (Either SomeException ()) -> IO ()
	robustly e 0 _ = error $ "failed to commit changes to sqlite database: " ++ show e
	robustly _ n a = do
		r <- a
		case r of
			Right _ -> return ()
			Left e -> do
				threadDelay 100000 -- 1/10th second
				robustly (Just e) (n-1) a

commitDb :: DbHandle -> SqlPersistM () -> IO (Either SomeException ())
commitDb (DbHandle _ jobs _) a = do
	res <- newEmptyMVar
	putMVar jobs $ ChangeJob $ \runner ->
		liftIO $ putMVar res =<< tryNonAsync (runner a)
	takeMVar res
