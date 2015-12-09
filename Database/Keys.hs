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
	flushDb,
	closeDb,
	addAssociatedFile,
	getAssociatedFiles,
	removeAssociatedFile,
	setInodeCache,
	getInodeCache,
	AssociatedId,
	DataId,
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

import Database.Persist.TH
import Database.Esqueleto hiding (Key)

share [mkPersist sqlSettings, mkMigrate "migrateKeysDb"] [persistLowerCase|
Associated
  key SKey
  file FilePath
  KeyFileIndex key file
Data
  key SKey
  inodeCache SInodeCache
  KeyIndex key
|]

{- Opens the database, creating it if it doesn't exist yet. -}
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
	h <- liftIO $ H.openDb db "data"

	-- work around https://github.com/yesodweb/persistent/issues/474
	liftIO setConsoleEncoding

	return $ DbHandle h

closeDb :: DbHandle -> IO ()
closeDb (DbHandle h) = H.closeDb h

withDbHandle :: (H.DbHandle -> IO a) -> Annex a
withDbHandle a = do
	(DbHandle h) <- dbHandle
	liftIO $ a h

dbHandle :: Annex DbHandle
dbHandle = maybe startup return =<< Annex.getState Annex.keysdbhandle
  where
	startup = do
		h <- openDb
		Annex.changeState $ \s -> s { Annex.keysdbhandle = Just h }
		return h

{- Flushes any changes made to the database. -}
flushDb :: Annex ()
flushDb = withDbHandle H.flushQueueDb

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

removeAssociatedFile :: Key -> FilePath -> Annex ()
removeAssociatedFile k f =  withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $
	delete $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk &&. r ^. AssociatedFile ==. val f)
  where
	sk = toSKey k

setInodeCache :: Key -> InodeCache -> Annex ()
setInodeCache k i = withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $
	void $ upsert (Data (toSKey k) (toSInodeCache i)) []

getInodeCache :: Key -> Annex (Maybe (InodeCache))
getInodeCache k = withDbHandle $ \h -> H.queryDb h $ do
	l <- select $ from $ \r -> do
		where_ (r ^. DataKey ==. val sk)
		return (r ^. DataInodeCache)
	return $ headMaybe $ map (fromSInodeCache . unValue) l
  where
	sk = toSKey k
