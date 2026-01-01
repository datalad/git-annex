{- Persistent sqlite database initialization
 -
 - Copyright 2015-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Database.Init where

import Annex.Common
import Annex.Perms
import Utility.FileMode
import qualified Utility.RawFilePath as R
import Database.RawFilePath

import Database.Persist.Sqlite
import Lens.Micro
import qualified Data.Text as T

{- Ensures that the database is freshly initialized. Deletes any
 - existing database. Pass the migration action for the database.
 -
 - The permissions of the database are set based on the
 - core.sharedRepository setting. Setting these permissions on the main db
 - file causes Sqlite to always use the same permissions for additional
 - files it writes later on
 -}
initDb :: OsPath -> SqlPersistM () -> Annex ()
initDb db migration = do
	let dbdir = takeDirectory db
	let tmpdbdir = dbdir <> literalOsPath ".tmp"
	let tmpdb = tmpdbdir </> literalOsPath "db"
	let tmpdb' = fromOsPath tmpdb
	createAnnexDirectory tmpdbdir
	liftIO $ runSqliteInfo' tmpdb' (enableWAL tmpdb) migration
	setAnnexDirPerm tmpdbdir
	-- Work around sqlite bug that prevents it from honoring
	-- less restrictive umasks.
	liftIO $ R.setFileMode tmpdb' =<< defaultFileMode
	setAnnexFilePerm tmpdb
	liftIO $ do
		void $ tryIO $ removeDirectoryRecursive dbdir
		R.rename (fromOsPath tmpdbdir) (fromOsPath dbdir)

{- Make sure that the database uses WAL mode, to prevent readers
 - from blocking writers, and prevent a writer from blocking readers.
 -
 - This is the default in recent persistent-sqlite versions, but 
 - force it on just in case. 
 -
 - Note that once WAL mode is enabled, it will persist whenever the
 - database is opened. -}
enableWAL :: OsPath -> SqliteConnectionInfo
enableWAL db = over walEnabled (const True) $ 
	mkSqliteConnectionInfo (T.pack (fromOsPath db))
