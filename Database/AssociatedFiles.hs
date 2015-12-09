{- Sqlite database used for tracking a key's associated files.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Database.AssociatedFiles (
	DbHandle,
	openDb,
	flushDb,
	closeDb,
	addDb,
	getDb,
	removeDb,
	AssociatedId,
) where

import Database.Types
import Database.AssociatedFiles.Types
import qualified Database.Handle as H
import Locations
import Common hiding (delete)
import Annex
import Types.Key
import Annex.Perms
import Annex.LockFile
import Messages

import Database.Persist.TH
import Database.Esqueleto hiding (Key)

share [mkPersist sqlSettings, mkMigrate "migrateAssociated"] [persistLowerCase|
Associated
  key SKey
  file FilePath
  KeyFileIndex key file
|]

{- Opens the database, creating it if it doesn't exist yet. -}
openDb :: Annex DbHandle
openDb = withExclusiveLock gitAnnexAssociatedFilesDbLock $ do
	dbdir <- fromRepo gitAnnexAssociatedFilesDb
	let db = dbdir </> "db"
	unlessM (liftIO $ doesFileExist db) $ do
		liftIO $ do
			createDirectoryIfMissing True dbdir
			H.initDb db $ void $
				runMigrationSilent migrateAssociated
		setAnnexDirPerm dbdir
		setAnnexFilePerm db
	h <- liftIO $ H.openDb db "associated"

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
dbHandle = maybe startup return =<< Annex.getState Annex.associatedfilesdbhandle
  where
	startup = do
		h <- openDb
		Annex.changeState $ \s -> s { Annex.associatedfilesdbhandle = Just h }
		return h

{- Flushes any changes made to the database. -}
flushDb :: Annex ()
flushDb = withDbHandle H.flushQueueDb

addDb :: Key -> FilePath -> Annex ()
addDb k f = withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $ do
	-- If the same file was associated with a different key before,
	-- remove that.
	delete $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val f &&. r ^. AssociatedKey ==. val sk)
	void $ insertUnique $ Associated sk f
  where
	sk = toSKey k

{- Note that the files returned used to be associated with the key, but
 - some of them may not be any longer. -}
getDb :: Key -> Annex [FilePath]
getDb k = withDbHandle $ \h -> H.queryDb h $ getDb' $ toSKey k

getDb' :: SKey -> SqlPersistM [FilePath]
getDb' sk = do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk)
		return (r ^. AssociatedFile)
	return $ map unValue l

removeDb :: Key -> FilePath -> Annex ()
removeDb k f =  withDbHandle $ \h -> H.queueDb h (\_ _ -> pure True) $
	delete $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk &&. r ^. AssociatedFile ==. val f)
  where
	sk = toSKey k
