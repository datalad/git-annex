{- Persistent sqlite database initialization
 -
 - Copyright 2015-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Database.Init where

import Annex.Common
import Annex.Perms
import Utility.FileMode

import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
#if MIN_VERSION_persistent_sqlite(2,6,2)
import Lens.Micro
#else
import qualified Database.Sqlite as Sqlite
#endif

{- Ensures that the database is freshly initialized. Deletes any
 - existing database. Pass the migration action for the database.
 -
 - The permissions of the database are set based on the
 - core.sharedRepository setting. Setting these permissions on the main db
 - file causes Sqlite to always use the same permissions for additional
 - files it writes later on
 -}
initDb :: FilePath -> SqlPersistM () -> Annex ()
initDb db migration = do
	let dbdir = takeDirectory db
	let tmpdbdir = dbdir ++ ".tmp"
	let tmpdb = tmpdbdir </> "db"
	let tdb = T.pack tmpdb	
	liftIO $ do
		createDirectoryIfMissing True tmpdbdir
#if MIN_VERSION_persistent_sqlite(2,6,2)
		runSqliteInfo (enableWAL tdb) migration
#else
		enableWAL tdb
		runSqlite tdb migration
#endif
	setAnnexDirPerm tmpdbdir
	-- Work around sqlite bug that prevents it from honoring
	-- less restrictive umasks.
	liftIO $ setFileMode tmpdb =<< defaultFileMode
	setAnnexFilePerm tmpdb
	liftIO $ do
		void $ tryIO $ removeDirectoryRecursive dbdir
		rename tmpdbdir dbdir

{- Make sure that the database uses WAL mode, to prevent readers
 - from blocking writers, and prevent a writer from blocking readers.
 -
 - This is the default in recent persistent-sqlite versions, but 
 - force it on just in case. 
 -
 - Note that once WAL mode is enabled, it will persist whenever the
 - database is opened. -}
#if MIN_VERSION_persistent_sqlite(2,6,2)
enableWAL :: T.Text -> SqliteConnectionInfo
enableWAL db = over walEnabled (const True) $ 
	mkSqliteConnectionInfo db
#else
enableWAL :: T.Text -> IO ()
enableWAL db = do
	conn <- Sqlite.open db
	stmt <- Sqlite.prepare conn (T.pack "PRAGMA journal_mode=WAL;")
	void $ Sqlite.step stmt
	void $ Sqlite.finalize stmt
	Sqlite.close conn
#endif
