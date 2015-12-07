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
	closeDb,
	addDb,
	getDb,
	removeDb,
	AssociatedId,
) where

import Database.Types
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

newtype DbHandle = DbHandle H.DbHandle

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

addDb :: DbHandle -> Key -> FilePath -> IO ()
addDb (DbHandle h) k f = H.queueDb h (\_ _ -> pure True) $ do
	-- If the same file was associated with a different key before,
	-- remove that.
	delete $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val f &&. r ^. AssociatedKey ==. val sk)
	void $ insertUnique $ Associated sk f
  where
	sk = toSKey k

{- Note that the files returned used to be associated with the key, but
 - some of them may not be any longer. -}
getDb :: DbHandle -> Key -> IO [FilePath]
getDb (DbHandle h) = H.queryDb h . getDb' . toSKey

getDb' :: SKey -> SqlPersistM [FilePath]
getDb' sk = do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk)
		return (r ^. AssociatedFile)
	return $ map unValue l

removeDb :: DbHandle -> Key -> FilePath -> IO ()
removeDb (DbHandle h) k f =  H.queueDb h (\_ _ -> pure True) $
	delete $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk &&. r ^. AssociatedFile ==. val f)
  where
	sk = toSKey k
