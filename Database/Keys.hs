{- Sqlite database of information about Keys
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Database.Keys (
	DbHandle,
	openDb,
	closeDb,
	addAssociatedFile,
	getAssociatedFiles,
	getAssociatedKey,
	removeAssociatedFile,
	storeInodeCaches,
	addInodeCaches,
	getInodeCaches,
	removeInodeCaches,
	AssociatedId,
	ContentId,
) where

import Database.Types
import Database.Keys.Types
import qualified Database.Handle as H
import Locations
import Common hiding (delete)
import Annex
import Types.Key
import Annex.Perms
import Annex.LockFile
import Messages
import Utility.InodeCache
import Annex.InodeSentinal

import Database.Persist.TH
import Database.Esqueleto hiding (Key)

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

{- Opens the database, creating it if it doesn't exist yet.
 -
 - Multiple readers and writers can have the database open at the same
 - time. Database.Handle deals with the concurrency issues.
 - The lock is held while opening the database, so that when
 - the database doesn't exist yet, one caller wins the lock and
 - can create it undisturbed.
 -}
openDb :: Annex DbHandle
openDb = withExclusiveLock gitAnnexKeysDbLock $ do
	dbdir <- fromRepo gitAnnexKeysDb
	let db = dbdir </> "db"
	unlessM (liftIO $ doesFileExist db) $ do
		liftIO $ do
			createDirectoryIfMissing True dbdir
			H.initDb db $ void $
				runMigrationSilent migrateKeysDb
		setAnnexDirPerm dbdir
		setAnnexFilePerm db
	h <- liftIO $ H.openDb db "content"

	-- work around https://github.com/yesodweb/persistent/issues/474
	liftIO setConsoleEncoding

	return $ DbHandle h

closeDb :: DbHandle -> IO ()
closeDb (DbHandle h) = H.closeDb h

withDbHandle :: (H.DbHandle -> IO a) -> Annex a
withDbHandle a = bracket openDb (liftIO . closeDb) (\(DbHandle h) -> liftIO (a h))

addAssociatedFile :: Key -> FilePath -> Annex ()
addAssociatedFile k f = withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $ do
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
getAssociatedFiles k = withDbHandle $ \h -> H.queryDb h $
	getAssociatedFiles' $ toSKey k

getAssociatedFiles' :: SKey -> SqlPersistM [FilePath]
getAssociatedFiles' sk = do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk)
		return (r ^. AssociatedFile)
	return $ map unValue l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: FilePath -> Annex [Key]
getAssociatedKey f = withDbHandle $ \h -> H.queryDb h $
	getAssociatedKey' f

getAssociatedKey' :: FilePath -> SqlPersistM [Key]
getAssociatedKey' f = do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val f)
		return (r ^. AssociatedKey)
	return $ map (fromSKey . unValue) l

removeAssociatedFile :: Key -> FilePath -> Annex ()
removeAssociatedFile k f = withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $
	delete $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk &&. r ^. AssociatedFile ==. val f)
  where
	sk = toSKey k

{- Stats the files, and stores their InodeCaches. -}
storeInodeCaches :: Key -> [FilePath] -> Annex ()
storeInodeCaches k fs = withTSDelta $ \d ->
	addInodeCaches k . catMaybes =<< liftIO (mapM (`genInodeCache` d) fs)

addInodeCaches :: Key -> [InodeCache] -> Annex ()
addInodeCaches k is = withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $
	forM_ is $ \i -> insertUnique $ Content (toSKey k) (toSInodeCache i)

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: Key -> Annex [InodeCache]
getInodeCaches k = withDbHandle $ \h -> H.queryDb h $ do
	l <- select $ from $ \r -> do
		where_ (r ^. ContentKey ==. val sk)
		return (r ^. ContentCache)
	return $ map (fromSInodeCache . unValue) l
  where
	sk = toSKey k

removeInodeCaches :: Key -> Annex ()
removeInodeCaches k = withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $
	delete $ from $ \r -> do
		where_ (r ^. ContentKey ==. val sk)
  where
	sk = toSKey k
