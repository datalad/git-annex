{- Sqlite database of information about Keys
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Database.Keys (
	DbHandle,
	closeDb,
	addAssociatedFile,
	getAssociatedFiles,
	getAssociatedKey,
	removeAssociatedFile,
	storeInodeCaches,
	addInodeCaches,
	getInodeCaches,
	removeInodeCaches,
	runWriter,
) where

import qualified Database.Keys.SQL as SQL
import Database.Types
import Database.Keys.Handle
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Annex.Common hiding (delete)
import Annex.Version (versionUsesKeysDatabase)
import qualified Annex
import Annex.LockFile
import Utility.InodeCache
import Annex.InodeSentinal
import Git.FilePath

{- Runs an action that reads from the database.
 -
 - If the database doesn't already exist, it's not created; mempty is
 - returned instead. This way, when the keys database is not in use,
 - there's minimal overhead in checking it.
 -
 - If the database is already open, any writes are flushed to it, to ensure
 - consistency.
 -
 - Any queued writes will be flushed before the read.
 -}
runReader :: Monoid v => (SQL.ReadHandle -> Annex v) -> Annex v
runReader a = do
	h <- getDbHandle
	withDbState h go
  where
	go DbUnavailable = return (mempty, DbUnavailable)
	go st@(DbOpen qh) = do
		liftIO $ H.flushDbQueue qh
		v <- a (SQL.ReadHandle qh)
		return (v, st)
	go DbClosed = do
		st' <- openDb False DbClosed
		v <- case st' of
			(DbOpen qh) -> a (SQL.ReadHandle qh)
			_ -> return mempty
		return (v, st')

runReaderIO :: Monoid v => (SQL.ReadHandle -> IO v) -> Annex v
runReaderIO a = runReader (liftIO . a)

{- Runs an action that writes to the database. Typically this is used to
 - queue changes, which will be flushed at a later point.
 -
 - The database is created if it doesn't exist yet. -}
runWriter :: (SQL.WriteHandle -> Annex ()) -> Annex ()
runWriter a = do
	h <- getDbHandle
	withDbState h go
  where
	go st@(DbOpen qh) = do
		v <- a (SQL.WriteHandle qh)
		return (v, st)
	go st = do
		st' <- openDb True st
		v <- case st' of
			DbOpen qh -> a (SQL.WriteHandle qh)
			_ -> error "internal"
		return (v, st')

runWriterIO :: (SQL.WriteHandle -> IO ()) -> Annex ()
runWriterIO a = runWriter (liftIO . a)

{- Gets the handle cached in Annex state; creates a new one if it's not yet
 - available, but doesn't open the database. -}
getDbHandle :: Annex DbHandle
getDbHandle = go =<< Annex.getState Annex.keysdbhandle
  where
	go (Just h) = pure h
	go Nothing = do
		h <- ifM versionUsesKeysDatabase
			( liftIO newDbHandle
			, liftIO unavailableDbHandle
			)
		Annex.changeState $ \s -> s { Annex.keysdbhandle = Just h }
		return h

{- Opens the database, perhaps creating it if it doesn't exist yet.
 -
 - Multiple readers and writers can have the database open at the same
 - time. Database.Handle deals with the concurrency issues.
 - The lock is held while opening the database, so that when
 - the database doesn't exist yet, one caller wins the lock and
 - can create it undisturbed.
 -}
openDb :: Bool -> DbState -> Annex DbState
openDb _ st@(DbOpen _) = return st
openDb False DbUnavailable = return DbUnavailable
openDb createdb _ = catchPermissionDenied permerr $ withExclusiveLock gitAnnexKeysDbLock $ do
	dbdir <- fromRepo gitAnnexKeysDb
	let db = dbdir </> "db"
	dbexists <- liftIO $ doesFileExist db
	case (dbexists, createdb) of
		(True, _) -> open db
		(False, True) -> do
			initDb db SQL.createTables
			open db
		(False, False) -> return DbUnavailable
  where
	open db = liftIO $ DbOpen <$> H.openDbQueue H.MultiWriter db SQL.containedTable
	-- If permissions don't allow opening the database, treat it as if
	-- it does not exist.
	permerr e = case createdb of
		False -> return DbUnavailable
		True -> throwM e

{- Closes the database if it was open. Any writes will be flushed to it.
 -
 - This does not normally need to be called; the database will auto-close
 - when the handle is garbage collected. However, this can be used to
 - force a re-read of the database, in case another process has written
 - data to it.
 -}
closeDb :: Annex ()
closeDb = liftIO . closeDbHandle =<< getDbHandle

addAssociatedFile :: Key -> TopFilePath -> Annex ()
addAssociatedFile k f = runWriterIO $ SQL.addAssociatedFile (toIKey k) f

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> Annex [TopFilePath]
getAssociatedFiles = runReaderIO . SQL.getAssociatedFiles . toIKey

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> Annex [Key]
getAssociatedKey = map fromIKey <$$> runReaderIO . SQL.getAssociatedKey

removeAssociatedFile :: Key -> TopFilePath -> Annex ()
removeAssociatedFile k = runWriterIO . SQL.removeAssociatedFile (toIKey k)

{- Stats the files, and stores their InodeCaches. -}
storeInodeCaches :: Key -> [FilePath] -> Annex ()
storeInodeCaches k fs = withTSDelta $ \d ->
	addInodeCaches k . catMaybes =<< liftIO (mapM (`genInodeCache` d) fs)

addInodeCaches :: Key -> [InodeCache] -> Annex ()
addInodeCaches k is = runWriterIO $ SQL.addInodeCaches (toIKey k) is

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: Key -> Annex [InodeCache]
getInodeCaches = runReaderIO . SQL.getInodeCaches . toIKey

removeInodeCaches :: Key -> Annex ()
removeInodeCaches = runWriterIO . SQL.removeInodeCaches . toIKey
