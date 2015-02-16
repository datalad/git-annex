{- Persistent sqlite database handles.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Database.Handle (
	DbHandle,
	openDb,
	closeDb,
	runDb,
) where

import Utility.Exception

import Database.Persist.Sqlite (runSqlite)
import Database.Esqueleto hiding (Key)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throwIO)
import qualified Data.Text as T

{- A DbHandle is a reference to a worker thread that communicates with
 - the database. It has a MVar which Jobs are submitted to. -}
data DbHandle = DbHandle (Async ()) (MVar Job)

data Job = Job (SqlPersistM ()) | CloseJob

openDb :: FilePath -> IO DbHandle
openDb db = do
	jobs <- newEmptyMVar
	worker <- async (workerThread db jobs)
	return $ DbHandle worker jobs

workerThread :: FilePath -> MVar Job -> IO ()
workerThread db jobs = runSqlite (T.pack db) go
  where
	go = do
		job <- liftIO $ takeMVar jobs
		case job of
			Job a -> a >> go
			CloseJob -> return ()

closeDb :: DbHandle -> IO ()
closeDb (DbHandle worker jobs) = do
	putMVar jobs CloseJob
	wait worker

{- Runs an action using the DbHandle.
 -
 - Note that the action is not run by the calling thread, but by a
 - worker thread. Exceptions are propigated to the calling thread.
 -
 - Note that only one action can be run at a time against a given DbHandle.
 - If called concurrently, this will block until it is able to run.
 -}
runDb :: DbHandle -> SqlPersistM a -> IO a
runDb (DbHandle _ jobs) a = do
	res <- newEmptyMVar
	putMVar jobs $ Job $ liftIO . putMVar res =<< tryNonAsync a
	either throwIO return =<< takeMVar res
