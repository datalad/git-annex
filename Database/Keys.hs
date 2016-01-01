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
	AssociatedId,
	ContentId,
) where

import Database.Types
import Database.Keys.Handle
import qualified Database.Queue as H
import Locations
import Common hiding (delete)
import Annex
import Types.Key
import Annex.Perms
import Annex.LockFile
import Utility.InodeCache
import Annex.InodeSentinal
import qualified Git.Types
import qualified Git.LsTree
import Git.Ref
import Git.FilePath
import Annex.CatFile
import Messages

import Database.Persist.TH
import Database.Esqueleto hiding (Key)
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateKeysDb"] [persistLowerCase|
Associated
  key SKey
  file FilePath
  KeyFileIndex key file
Content
  key SKey
  cache SInodeCache
  KeyCacheIndex key cache
|]

newtype ReadHandle = ReadHandle H.DbQueue

type Reader v = ReadHandle -> Annex v

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
runReader :: Monoid v => Reader v -> Annex v
runReader a = do
	h <- getDbHandle
	withDbState h go
  where
	go DbEmpty = return (mempty, DbEmpty)
	go st@(DbOpen qh) = do
		liftIO $ H.flushDbQueue qh
		v <- a (ReadHandle qh)
		return (v, st)
	go DbClosed = do
		st' <- openDb False DbClosed
		v <- case st' of
			(DbOpen qh) -> a (ReadHandle qh)
			_ -> return mempty
		return (v, st')

readDb :: SqlPersistM a -> ReadHandle -> Annex a
readDb a (ReadHandle h) = liftIO $ H.queryDbQueue h a

newtype WriteHandle = WriteHandle H.DbQueue

type Writer = WriteHandle -> Annex ()

{- Runs an action that writes to the database. Typically this is used to
 - queue changes, which will be flushed at a later point.
 -
 - The database is created if it doesn't exist yet. -}
runWriter :: Writer -> Annex ()
runWriter a = do
	h <- getDbHandle
	withDbState h go
  where
	go st@(DbOpen qh) = do
		v <- a (WriteHandle qh)
		return (v, st)
	go st = do
		st' <- openDb True st
		v <- case st' of
			DbOpen qh -> a (WriteHandle qh)
			_ -> error "internal"
		return (v, st')

queueDb :: SqlPersistM () -> WriteHandle -> Annex ()
queueDb a (WriteHandle h) = liftIO $ H.queueDb h checkcommit a
  where
	-- commit queue after 1000 changes or 5 minutes, whichever comes first
	checkcommit sz lastcommittime
		| sz > 1000 = return True
		| otherwise = do
			now <- getCurrentTime
			return $ diffUTCTime lastcommittime now > 300

{- Gets the handle cached in Annex state; creates a new one if it's not yet
 - available, but doesn't open the database. -}
getDbHandle :: Annex DbHandle
getDbHandle = go =<< getState keysdbhandle
  where
	go (Just h) = pure h
	go Nothing = do
		h <- liftIO newDbHandle
		changeState $ \s -> s { keysdbhandle = Just h }
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
				H.initDb db $ void $
					runMigrationSilent migrateKeysDb
			setAnnexDirPerm dbdir
			setAnnexFilePerm db
			open db
		(False, False) -> return DbEmpty
  where
	open db = liftIO $ DbOpen <$> H.openDbQueue db "content"

addAssociatedFile :: Key -> FilePath -> Annex ()
addAssociatedFile k f = runWriter $ addAssociatedFile' k f

addAssociatedFile' :: Key -> FilePath -> Writer
addAssociatedFile' k f = queueDb $ do
	-- If the same file was associated with a different key before,
	-- remove that.
	delete $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val f &&. r ^. AssociatedKey ==. val sk)
	void $ insertUnique $ Associated sk f
  where
	sk = toSKey k

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> Annex [FilePath]
getAssociatedFiles = runReader . getAssociatedFiles' . toSKey

getAssociatedFiles' :: SKey -> Reader [FilePath]
getAssociatedFiles' sk = readDb $ do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk)
		return (r ^. AssociatedFile)
	return $ map unValue l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: FilePath -> Annex [Key]
getAssociatedKey = runReader . getAssociatedKey'

getAssociatedKey' :: FilePath -> Reader [Key]
getAssociatedKey' f = readDb $ do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val f)
		return (r ^. AssociatedKey)
	return $ map (fromSKey . unValue) l

removeAssociatedFile :: Key -> FilePath -> Annex ()
removeAssociatedFile k = runWriter . removeAssociatedFile' (toSKey k)

removeAssociatedFile' :: SKey -> FilePath -> Writer
removeAssociatedFile' sk f = queueDb $ 
	delete $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk &&. r ^. AssociatedFile ==. val f)
	
{- Find all unlocked associated files. This is expensive, and so normally
 - the associated files are updated incrementally when changes are noticed. -}
scanAssociatedFiles :: Annex ()
scanAssociatedFiles = runWriter $ \h -> do
	showSideAction "scanning for unlocked files"
	dropallassociated h
	l <- inRepo $ Git.LsTree.lsTree headRef
	forM_ l $ \i -> 
		when (isregfile i) $
			maybe noop (add h i)
				=<< catKey (Git.Types.Ref $ Git.LsTree.sha i)
  where
	dropallassociated = queueDb $
		delete $ from $ \(_r :: SqlExpr (Entity Associated)) ->
			return ()
	isregfile i = Git.Types.toBlobType (Git.LsTree.mode i) == Just Git.Types.FileBlob
	add h i k = flip queueDb h $ 
		void $ insertUnique $ Associated
			(toSKey k)
			(getTopFilePath $ Git.LsTree.file i)

{- Stats the files, and stores their InodeCaches. -}
storeInodeCaches :: Key -> [FilePath] -> Annex ()
storeInodeCaches k fs = withTSDelta $ \d ->
	addInodeCaches k . catMaybes =<< liftIO (mapM (`genInodeCache` d) fs)

addInodeCaches :: Key -> [InodeCache] -> Annex ()
addInodeCaches k is = runWriter $ addInodeCaches' (toSKey k) is

addInodeCaches' :: SKey -> [InodeCache] -> Writer
addInodeCaches' sk is = queueDb $
	forM_ is $ \i -> insertUnique $ Content sk (toSInodeCache i)

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: Key -> Annex [InodeCache]
getInodeCaches = runReader . getInodeCaches' . toSKey

getInodeCaches' :: SKey -> Reader [InodeCache]
getInodeCaches' sk = readDb $ do
	l <- select $ from $ \r -> do
		where_ (r ^. ContentKey ==. val sk)
		return (r ^. ContentCache)
	return $ map (fromSInodeCache . unValue) l

removeInodeCaches :: Key -> Annex ()
removeInodeCaches = runWriter . removeInodeCaches' . toSKey

removeInodeCaches' :: SKey -> Writer
removeInodeCaches' sk = queueDb $ 
	delete $ from $ \r -> do
		where_ (r ^. ContentKey ==. val sk)
