{- Persistent sqlite database handles.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Database.Handle (
	DbHandle,
	DbConcurrency(..),
	openDb,
	TableName,
	queryDb,
	closeDb,
	commitDb,
	commitDb',
) where

import Utility.Exception
import Utility.FileSystemEncoding

import Database.Persist.Sqlite
import qualified Database.Sqlite as Sqlite
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throwIO, BlockedIndefinitelyOnMVar(..))
import qualified Data.Text as T
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import Data.List
import System.IO

{- A DbHandle is a reference to a worker thread that communicates with
 - the database. It has a MVar which Jobs are submitted to. -}
data DbHandle = DbHandle DbConcurrency (Async ()) (MVar Job)

{- Name of a table that should exist once the database is initialized. -}
type TableName = String

{- Sqlite only allows a single write to a database at a time; a concurrent
 - write will crash. 
 -
 - While a DbHandle serializes concurrent writes from
 - multiple threads. But, when a database can be written to by
 - multiple processes concurrently, use MultiWriter to make writes
 - to the database be done robustly.
 - 
 - The downside of using MultiWriter is that after writing a change to the
 - database, the a query using the same DbHandle will not immediately see
 - the change! This is because the change is actually written using a
 - separate database connection, and caching can prevent seeing the change.
 - Also, consider that if multiple processes are writing to a database,
 - you can't rely on seeing values you've just written anyway, as another
 - process may change them.
 -
 - When a database can only be written to by a single process, use
 - SingleWriter. Changes written to the database will always be immediately
 - visible then.
 -}
data DbConcurrency = SingleWriter | MultiWriter

{- Opens the database, but does not perform any migrations. Only use
 - once the database is known to exist and have the right tables. -}
openDb :: DbConcurrency -> FilePath -> TableName -> IO DbHandle
openDb dbconcurrency db tablename = do
	jobs <- newEmptyMVar
	worker <- async (workerThread (T.pack db) tablename jobs)
	
	-- work around https://github.com/yesodweb/persistent/issues/474
	liftIO $ fileEncoding stderr

	return $ DbHandle dbconcurrency worker jobs

{- This is optional; when the DbHandle gets garbage collected it will
 - auto-close. -}
closeDb :: DbHandle -> IO ()
closeDb (DbHandle _ worker jobs) = do
	putMVar jobs CloseJob
	wait worker

{- Makes a query using the DbHandle. This should not be used to make
 - changes to the database!
 -
 - Note that the action is not run by the calling thread, but by a
 - worker thread. Exceptions are propigated to the calling thread.
 -
 - Only one action can be run at a time against a given DbHandle.
 - If called concurrently in the same process, this will block until
 - it is able to run.
 -
 - Note that when the DbHandle was opened in MultiWriter mode, recent
 - writes may not be seen by queryDb.
 -}
queryDb :: DbHandle -> SqlPersistM a -> IO a
queryDb (DbHandle _ _ jobs) a = do
	res <- newEmptyMVar
	putMVar jobs $ QueryJob $
		liftIO . putMVar res =<< tryNonAsync a
	(either throwIO return =<< takeMVar res)
		`catchNonAsync` (const $ error "sqlite query crashed")

{- Writes a change to the database.
 -
 - In MultiWriter mode, catches failure to write to the database,
 - and retries repeatedly for up to 10 seconds,  which should avoid
 - all but the most exceptional problems.
 -}
commitDb :: DbHandle -> SqlPersistM () -> IO ()
commitDb h wa = robustly Nothing 100 (commitDb' h wa)
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

commitDb' :: DbHandle -> SqlPersistM () -> IO (Either SomeException ())
commitDb' (DbHandle MultiWriter _ jobs) a = do
	res <- newEmptyMVar
	putMVar jobs $ RobustChangeJob $ \runner ->
		liftIO $ putMVar res =<< tryNonAsync (runner a)
	takeMVar res
commitDb' (DbHandle SingleWriter _ jobs) a = do
	res <- newEmptyMVar
	putMVar jobs $ ChangeJob $
		liftIO . putMVar res =<< tryNonAsync a
	takeMVar res
		`catchNonAsync` (const $ error "sqlite commit crashed")

data Job
	= QueryJob (SqlPersistM ())
	| ChangeJob (SqlPersistM ())
	| RobustChangeJob ((SqlPersistM () -> IO ()) -> IO ())
	| CloseJob

workerThread :: T.Text -> TableName -> MVar Job -> IO ()
workerThread db tablename jobs = go
  where
	go = do
		v <- tryNonAsync (runSqliteRobustly tablename db loop)
		case v of
			Left e -> hPutStrLn stderr $
				"sqlite worker thread crashed: " ++ show e
			Right True -> go
			Right False -> return ()
	
	getjob :: IO (Either BlockedIndefinitelyOnMVar Job)
	getjob = try $ takeMVar jobs

	loop = do
		job <- liftIO getjob
		case job of
			-- Exception is thrown when the MVar is garbage
			-- collected, which means the whole DbHandle
			-- is not used any longer. Shutdown cleanly.
			Left BlockedIndefinitelyOnMVar -> return False
			Right CloseJob -> return False
			Right (QueryJob a) -> a >> loop
			Right (ChangeJob a) -> do
				a
				-- Exit this sqlite transaction so the
				-- database gets updated on disk.
				return True
			-- Change is run in a separate database connection
			-- since sqlite only supports a single writer at a
			-- time, and it may crash the database connection
			-- that the write is made to.
			Right (RobustChangeJob a) -> do
				liftIO (a (runSqliteRobustly tablename db))
				loop
	
-- like runSqlite, but calls settle on the raw sql Connection.
runSqliteRobustly :: TableName -> T.Text -> (SqlPersistM a) -> IO a
runSqliteRobustly tablename db a = do
	conn <- Sqlite.open db
	settle conn
	runResourceT $ runNoLoggingT $
		withSqlConn (wrapConnection conn) $
			runSqlConn a
  where
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
