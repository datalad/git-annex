{- Sqlite database of information about Keys
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Database.Keys (
	DbHandle,
	addAssociatedFile,
	getAssociatedFiles,
	getAssociatedKey,
	removeAssociatedFile,
	scanAssociatedFiles,
	storeInodeCaches,
	addInodeCaches,
	getInodeCaches,
	removeInodeCaches,
) where

import qualified Database.Keys.SQL as SQL
import Database.Types
import Database.Keys.Handle
import qualified Database.Queue as H
import Locations
import Common.Annex hiding (delete)
import qualified Annex
import Annex.Perms
import Annex.LockFile
import Utility.InodeCache
import Annex.InodeSentinal
import qualified Git.Types
import qualified Git.LsTree
import qualified Git.Branch
import Git.Ref
import Git.FilePath
import Annex.CatFile

import Database.Esqueleto hiding (Key)

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
	go DbEmpty = return (mempty, DbEmpty)
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
		h <- liftIO newDbHandle
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
openDb False DbEmpty = return DbEmpty
openDb createdb _ = withExclusiveLock gitAnnexKeysDbLock $ do
	dbdir <- fromRepo gitAnnexKeysDb
	let db = dbdir </> "db"
	dbexists <- liftIO $ doesFileExist db
	case (dbexists, createdb) of
		(True, _) -> open db
		(False, True) -> do
			liftIO $ do
				createDirectoryIfMissing True dbdir
				H.initDb db SQL.createTables
			setAnnexDirPerm dbdir
			setAnnexFilePerm db
			open db
		(False, False) -> return DbEmpty
  where
	open db = liftIO $ DbOpen <$> H.openDbQueue db SQL.containedTable

addAssociatedFile :: Key -> TopFilePath -> Annex ()
addAssociatedFile k f = runWriterIO $ SQL.addAssociatedFile (toSKey k) f

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> Annex [TopFilePath]
getAssociatedFiles = runReaderIO . SQL.getAssociatedFiles . toSKey

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> Annex [Key]
getAssociatedKey = map fromSKey <$$> runReaderIO . SQL.getAssociatedKey

removeAssociatedFile :: Key -> TopFilePath -> Annex ()
removeAssociatedFile k = runWriterIO . SQL.removeAssociatedFile (toSKey k)

{- Find all unlocked associated files. This is expensive, and so normally
 - the associated files are updated incrementally when changes are noticed. -}
scanAssociatedFiles :: Annex ()
scanAssociatedFiles = whenM (isJust <$> inRepo Git.Branch.current) $ 
	runWriter $ \h -> do
		showSideAction "scanning for unlocked files"
		dropallassociated h
		(l, cleanup) <- inRepo $ Git.LsTree.lsTree headRef
		forM_ l $ \i -> 
			when (isregfile i) $
				maybe noop (add h i)
					=<< catKey (Git.LsTree.sha i)
		liftIO $ void cleanup
  where
	dropallassociated h = liftIO $ flip SQL.queueDb h $
		delete $ from $ \(_r :: SqlExpr (Entity SQL.Associated)) ->
			return ()
	isregfile i = Git.Types.toBlobType (Git.LsTree.mode i) == Just Git.Types.FileBlob
	add h i k = liftIO $ flip SQL.queueDb h $ 
		void $ insertUnique $ SQL.Associated
			(toSKey k)
			(getTopFilePath $ Git.LsTree.file i)

{- Stats the files, and stores their InodeCaches. -}
storeInodeCaches :: Key -> [FilePath] -> Annex ()
storeInodeCaches k fs = withTSDelta $ \d ->
	addInodeCaches k . catMaybes =<< liftIO (mapM (`genInodeCache` d) fs)

addInodeCaches :: Key -> [InodeCache] -> Annex ()
addInodeCaches k is = runWriterIO $ SQL.addInodeCaches (toSKey k) is

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: Key -> Annex [InodeCache]
getInodeCaches = runReaderIO . SQL.getInodeCaches . toSKey

removeInodeCaches :: Key -> Annex ()
removeInodeCaches = runWriterIO . SQL.removeInodeCaches . toSKey
