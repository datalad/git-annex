{- Persistent sqlite database initialization
 -
 - Copyright 2015-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Database.Init where

import Annex.Common
import Annex.Perms
import Utility.FileMode

import Database.Persist.Sqlite
import qualified Database.Sqlite as Sqlite
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

{- Ensures that the database is freshly initialized. Deletes any
 - existing database. Pass the migration action for the database.
 -
 - The database is initialized using WAL mode, to prevent readers
 - from blocking writers, and prevent a writer from blocking readers.
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
	liftIO $ do
		createDirectoryIfMissing True tmpdbdir
		let tdb = T.pack tmpdb
		enableWAL tdb
		runSqlite tdb migration
	setAnnexDirPerm tmpdbdir
	-- Work around sqlite bug that prevents it from honoring
	-- less restrictive umasks.
	liftIO $ setFileMode tmpdb =<< defaultFileMode
	setAnnexFilePerm tmpdb
	liftIO $ do
		void $ tryIO $ removeDirectoryRecursive dbdir
		rename tmpdbdir dbdir

enableWAL :: T.Text -> IO ()
enableWAL db = do
	conn <- Sqlite.open db
	stmt <- Sqlite.prepare conn (T.pack "PRAGMA journal_mode=WAL;")
	void $ Sqlite.step stmt
	void $ Sqlite.finalize stmt
	Sqlite.close conn
