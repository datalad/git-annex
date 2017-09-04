{- Sqlite database used for exports to special remotes.
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Database.Export (
	ExportHandle,
	openDb,
	closeDb,
	addExportLocation,
	removeExportLocation,
	flushDbQueue,
	getExportLocation,
	ExportedId,
) where

import Database.Types
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Annex.Common hiding (delete)
import Types.Remote (ExportLocation(..))

import Database.Persist.TH
import Database.Esqueleto hiding (Key)

newtype ExportHandle = ExportHandle H.DbQueue

share [mkPersist sqlSettings, mkMigrate "migrateExport"] [persistLowerCase|
Exported
  key IKey
  file SFilePath
  KeyFileIndex key file
|]

{- Opens the database, creating it if it doesn't exist yet. -}
openDb :: UUID -> Annex ExportHandle
openDb u = do
	dbdir <- fromRepo (gitAnnexExportDbDir u)
	let db = dbdir </> "db"
	unlessM (liftIO $ doesFileExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateExport
	h <- liftIO $ H.openDbQueue db "exported"
	return $ ExportHandle h

closeDb :: ExportHandle -> Annex ()
closeDb (ExportHandle h) = liftIO $ H.closeDbQueue h

queueDb :: ExportHandle -> SqlPersistM () -> IO ()
queueDb (ExportHandle h) = H.queueDb h checkcommit
  where
	-- commit queue after 1000 changes
	checkcommit sz _lastcommittime
		| sz > 1000 = return True
		| otherwise = return False

addExportLocation :: ExportHandle -> Key -> ExportLocation -> IO ()
addExportLocation h k (ExportLocation f) = queueDb h $ 
	void $ insertUnique $ Exported (toIKey k) (toSFilePath f)

removeExportLocation :: ExportHandle -> Key -> ExportLocation -> IO ()
removeExportLocation h k (ExportLocation f) = queueDb h $
	delete $ from $ \r -> do
		where_ (r ^. ExportedKey ==. val ik &&. r ^. ExportedFile ==. val ef)
  where
	ik = toIKey k
	ef = toSFilePath f

flushDbQueue :: ExportHandle -> IO ()
flushDbQueue (ExportHandle h) = H.flushDbQueue h

{- Note that this does not see recently queued changes. -}
getExportLocation :: ExportHandle -> Key -> IO [ExportLocation]
getExportLocation (ExportHandle h) k = H.queryDbQueue h $ do
	l <- select $ from $ \r -> do
		where_ (r ^. ExportedKey ==. val ik)
		return (r ^. ExportedFile)
	return $ map (ExportLocation . fromSFilePath . unValue) l
  where
	ik = toIKey k
