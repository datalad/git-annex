{- Persistent sqlite database handles.
 -
 - Copyright 2015-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, CPP #-}

module Database.Handle (
	DbHandle,
	openDb,
	TableName,
	queryDb,
	closeDb,
	commitDb,
	commitDb',
) where

import Utility.Exception
import Utility.FileSystemEncoding
import Utility.Debug
import Utility.DebugLocks
import Utility.InodeCache
import Utility.OsPath

import Database.Persist.Sqlite
import qualified Database.Sqlite as Sqlite
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (MonadLoggerIO, askLoggerIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throwIO, BlockedIndefinitelyOnMVar(..))
import qualified Data.Text as T
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import System.IO

{- A DbHandle is a reference to a worker thread that communicates with
 - the database. It has a MVar which Jobs are submitted to. 
 - There is also an MVar which it will fill when there is a fatal error-}
data DbHandle = DbHandle OsPath (Async ()) (MVar Job) (MVar String)

{- Name of a table that should exist once the database is initialized. -}
type TableName = String

{- Opens the database, but does not perform any migrations. Only use
 - once the database is known to exist and have the right tables. -}
openDb :: OsPath -> TableName -> IO DbHandle
openDb db tablename = do
	jobs <- newEmptyMVar
	errvar <- newEmptyMVar
	worker <- async (workerThread db tablename jobs errvar)
	
	-- work around https://github.com/yesodweb/persistent/issues/474
	liftIO $ fileEncoding stderr

	return $ DbHandle db worker jobs errvar

{- This is optional; when the DbHandle gets garbage collected it will
 - auto-close. -}
closeDb :: DbHandle -> IO ()
closeDb (DbHandle _db worker jobs _) = do
	debugLocks $ putMVar jobs CloseJob
	wait worker

{- Makes a query using the DbHandle. This should not be used to make
 - changes to the database!
 -
 - Note that the action is not run by the calling thread, but by a
 - worker thread. Exceptions are propagated to the calling thread.
 -
 - Only one action can be run at a time against a given DbHandle.
 - If called concurrently in the same process, this will block until
 - it is able to run.
 -}
queryDb :: DbHandle -> SqlPersistM a -> IO a
queryDb (DbHandle _db _ jobs errvar) a = do
	res <- newEmptyMVar
	putMVar jobs $ QueryJob $
		debugLocks $ liftIO . putMVar res =<< tryNonAsync a
	debugLocks $ takeMVarSafe res >>= \case
		Right r -> either throwIO return r
		Left BlockedIndefinitelyOnMVar -> do
			err <- takeMVar errvar
			giveup $ "sqlite worker thread crashed: " ++ err

{- Writes a change to the database.
 -
 - Writes can fail when another write is happening concurrently.
 - So write failures are caught and retried.
 -
 - Retries repeatedly for up to 60 seconds. Part that point, it continues
 - retrying only if the database shows signs of being modified by another
 - process at least once each 30 seconds.
 -}
commitDb :: DbHandle -> SqlPersistM () -> IO ()
commitDb h@(DbHandle db _ _ errvar) wa = 
	robustly (commitDb' h wa) maxretries emptyDatabaseInodeCache
  where
	robustly a retries ic = do
		r <- a
		case r of
			Right (Right _) -> return ()
			Right (Left err) -> do
				threadDelay briefdelay
				retryHelper "write to" err maxretries db retries ic $ 
					robustly a
			Left BlockedIndefinitelyOnMVar -> do
				err <- takeMVar errvar
				giveup $ "sqlite worker thread crashed: " ++ err
	
	briefdelay = 100000 -- 1/10th second

	maxretries = 300 :: Int -- 30 seconds of briefdelay

commitDb' :: DbHandle -> SqlPersistM () -> IO (Either BlockedIndefinitelyOnMVar (Either SomeException ()))
commitDb' (DbHandle _ _ jobs _) a = do
	debug "Database.Handle" "commitDb start"
	res <- newEmptyMVar
	putMVar jobs $ ChangeJob $
		debugLocks $ liftIO . putMVar res =<< tryNonAsync a
	r <- debugLocks $ takeMVarSafe res
	case r of
		Right (Right ()) -> debug "Database.Handle" "commitDb done"
		Right (Left e) -> debug "Database.Handle" ("commitDb failed: " ++ show e)
		Left BlockedIndefinitelyOnMVar -> debug "Database.Handle" "commitDb BlockedIndefinitelyOnMVar"

	return r

data Job
	= QueryJob (SqlPersistM ())
	| ChangeJob (SqlPersistM ())
	| CloseJob

workerThread :: OsPath -> TableName -> MVar Job -> MVar String -> IO ()
workerThread db tablename jobs errvar = newconn
  where
	newconn = do
		v <- tryNonAsync (runSqliteRobustly tablename db loop)
		case v of
			Left e -> putMVar errvar (show e)
			Right cont -> cont
	
	loop = do
		job <- liftIO (takeMVarSafe jobs)
		case job of
			-- Exception is thrown when the MVar is garbage
			-- collected, which means the whole DbHandle
			-- is not used any longer. Shutdown cleanly.
			Left BlockedIndefinitelyOnMVar -> return (return ())
			Right CloseJob -> return (return ())
			Right (QueryJob a) -> a >> loop
			Right (ChangeJob a) -> do
				a
				-- Exit the sqlite connection so the
				-- database gets updated on disk.
				return newconn

{- Like runSqlite, but more robust.
 -
 - New database connections can sometimes take a while to become usable,
 - and selects will fail with ErrorBusy in the meantime. This may be due to
 - WAL mode recovering after a crash, or a concurrent writer.
 - So, wait until a select succeeds; once one succeeds the connection will
 - stay usable.
 -
 - Also sqlite sometimes throws ErrorIO when there's not really an IO
 - problem, but perhaps just a short read(). So also retry on ErrorIO.
 -
 - Retries repeatedly for up to 60 seconds. Part that point, it continues
 - retrying only if the database shows signs of being modified by another
 - process at least once each 30 seconds.
 -}
runSqliteRobustly :: TableName -> OsPath -> (SqlPersistM a) -> IO a
runSqliteRobustly tablename db a = do
	conn <- opensettle maxretries emptyDatabaseInodeCache
	go conn maxretries emptyDatabaseInodeCache
  where
	go conn retries ic = do
		r <- try $ runResourceT $ runNoLoggingT $
			withSqlConnRobustly db (wrapConnection conn) $
				runSqlConn a
		case r of
			Right v -> return v
			Left ex@(Sqlite.SqliteException { Sqlite.seError = e })
				| e == Sqlite.ErrorIO -> do
					briefdelay
					retryHelper "access" ex maxretries db retries ic $
						go conn
				| otherwise -> rethrow $ errmsg "after successful open" ex
	
	opensettle retries ic = do
#if MIN_VERSION_persistent_sqlite(2,13,3)
		conn <- Sqlite.open' (fromOsPath db)
#else
		conn <- Sqlite.open (T.pack (fromOsPath db))
#endif
		settle conn retries ic

	settle conn retries ic = do
		r <- try $ do
			stmt <- Sqlite.prepare conn nullselect
			void $ Sqlite.step stmt
			void $ Sqlite.finalize stmt
		case r of
			Right _ -> return conn
			Left ex@(Sqlite.SqliteException { Sqlite.seError = e })
				| e == Sqlite.ErrorBusy || e == Sqlite.ErrorIO -> do
					when (e == Sqlite.ErrorIO) $
						Sqlite.close conn
					briefdelay
					retryHelper "open" ex maxretries db retries ic $
						if e == Sqlite.ErrorIO
							then opensettle
							else settle conn
				| otherwise -> rethrow $ errmsg "while opening database connection" ex
	
	-- This should succeed for any table.
	nullselect = T.pack $ "SELECT null from " ++ tablename ++ " limit 1"

	briefdelay = threadDelay 1000 -- 1/1000th second
	
	maxretries = 30000 :: Int -- 30 seconds of briefdelays
	
	rethrow = throwIO . userError

	errmsg msg e = show e ++ "(" ++ msg ++ ")"

-- Like withSqlConn, but more robust.
withSqlConnRobustly
	:: (MonadUnliftIO m
		, MonadLoggerIO m
		, IsPersistBackend backend
		, BaseBackend backend ~ SqlBackend
		, BackendCompatible SqlBackend backend
	    )
	=> OsPath
	-> (LogFunc -> IO backend)
	-> (backend -> m a)
	-> m a
withSqlConnRobustly db open f = do
	logFunc <- askLoggerIO
	withRunInIO $ \run -> bracket
		(open logFunc)
		(closeRobustly db)
		(run . f)

{- Sqlite can throw ErrorBusy while closing a database; this catches
 - the exception and retries.
 -
 - Retries repeatedly for up to 60 seconds. Part that point, it continues
 - retrying only if the database shows signs of being modified by another
 - process at least once each 30 seconds.
 -}
closeRobustly
	:: (IsPersistBackend backend
		, BaseBackend backend ~ SqlBackend
		, BackendCompatible SqlBackend backend
	   )
	=> OsPath
	-> backend
	-> IO ()
closeRobustly db conn = go maxretries emptyDatabaseInodeCache
  where
	go retries ic = do
		r <- try $ close' conn
		case r of
			Right () -> return ()
			Left ex@(Sqlite.SqliteException { Sqlite.seError = e })
				| e == Sqlite.ErrorBusy -> do
					threadDelay briefdelay
					retryHelper "close" ex maxretries db retries ic go
				| otherwise -> rethrow $ errmsg "while closing database connection" ex
	
	briefdelay = 1000 -- 1/1000th second
	
	maxretries = 30000 :: Int -- 30 seconds of briefdelays
	
	rethrow = throwIO . userError

	errmsg msg e = show e ++ "(" ++ msg ++ ")"

{- Retries a sqlite action repeatedly, but not forever. Detects situations
 - when another git-annex process is suspended and has the database locked,
 - and eventually gives up. The retries is the current number of retries
 - that are left. The maxretries is how many retries to make each time
 - the database is seen to have been modified by some other process.
 -}
retryHelper
	:: Show err 
	=> String
	-> err
	-> Int
	-> OsPath
	-> Int
	-> DatabaseInodeCache
	-> (Int -> DatabaseInodeCache -> IO a)
	-> IO a
retryHelper action err maxretries db retries ic a = do
	let retries' = retries - 1
	if retries' < 1
		then do
			ic' <- getDatabaseInodeCache db
			if isDatabaseModified ic ic'
				then a maxretries ic'
				else giveup (databaseAccessStalledMsg action db err)
		else a retries' ic

databaseAccessStalledMsg :: Show err => String -> OsPath -> err -> String
databaseAccessStalledMsg action db err =
	"Repeatedly unable to " ++ action ++ " sqlite database " ++ fromOsPath db 
		++ ": " ++ show err ++ ". "
		++ "Perhaps another git-annex process is suspended and is "
		++ "keeping this database locked?"

data DatabaseInodeCache = DatabaseInodeCache (Maybe InodeCache) (Maybe InodeCache)

emptyDatabaseInodeCache :: DatabaseInodeCache
emptyDatabaseInodeCache = DatabaseInodeCache Nothing Nothing

getDatabaseInodeCache :: OsPath -> IO DatabaseInodeCache
getDatabaseInodeCache db = DatabaseInodeCache
	<$> genInodeCache db noTSDelta
	<*> genInodeCache (db <> literalOsPath "-wal") noTSDelta

isDatabaseModified :: DatabaseInodeCache -> DatabaseInodeCache -> Bool
isDatabaseModified (DatabaseInodeCache a1 b1) (DatabaseInodeCache a2 b2) = 
	ismodified a1 a2 || ismodified b1 b2
  where
	ismodified (Just a) (Just b) = not (compareStrong a b)
	ismodified Nothing Nothing = False
	ismodified _ _ = True

takeMVarSafe :: MVar a -> IO (Either BlockedIndefinitelyOnMVar a)
takeMVarSafe = try . takeMVar
