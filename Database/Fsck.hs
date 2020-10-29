{- Sqlite database used for incremental fsck. 
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
#if MIN_VERSION_persistent_template(2,8,0)
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module Database.Fsck (
	FsckHandle,
	newPass,
	openDb,
	closeDb,
	addDb,
	inDb,
	FsckedId,
) where

import Database.Types
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Utility.Exception
import Annex.Common
import Annex.LockFile

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import Data.Time.Clock

data FsckHandle = FsckHandle H.DbQueue UUID

{- Each key stored in the database has already been fscked as part
 - of the latest incremental fsck pass. -}
share [mkPersist sqlSettings, mkMigrate "migrateFsck"] [persistLowerCase|
Fscked
  key Key
  FsckedKeyIndex key
|]

{- The database is removed when starting a new incremental fsck pass.
 -
 - (The old fsck database used before v8 is also removed here.)
 -
 - This may fail, if other fsck processes are currently running using the
 - database. Removing the database in that situation would lead to crashes
 - or unknown behavior.
 -}
newPass :: UUID -> Annex Bool
newPass u = isJust <$> tryExclusiveLock (gitAnnexFsckDbLock u) go
  where
	go = do
		removedb =<< fromRepo (gitAnnexFsckDbDir u)
		removedb =<< fromRepo (gitAnnexFsckDbDirOld u)
	removedb = liftIO . void . tryIO . removeDirectoryRecursive . fromRawFilePath

{- Opens the database, creating it if it doesn't exist yet. -}
openDb :: UUID -> Annex FsckHandle
openDb u = do
	dbdir <- fromRepo (gitAnnexFsckDbDir u)
	let db = fromRawFilePath dbdir </> "db"
	unlessM (liftIO $ doesFileExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateFsck
	lockFileCached =<< fromRepo (gitAnnexFsckDbLock u)
	h <- liftIO $ H.openDbQueue H.MultiWriter db "fscked"
	return $ FsckHandle h u

closeDb :: FsckHandle -> Annex ()
closeDb (FsckHandle h u) = do
	liftIO $ H.closeDbQueue h
	unlockFile =<< fromRepo (gitAnnexFsckDbLock u)

addDb :: FsckHandle -> Key -> IO ()
addDb (FsckHandle h _) k = H.queueDb h checkcommit $
	void $ insertUnique $ Fscked k
  where
	-- commit queue after 1000 files or 5 minutes, whichever comes first
	checkcommit sz lastcommittime
		| sz > 1000 = return True
		| otherwise = do
			now <- getCurrentTime
			return $ diffUTCTime now lastcommittime > 300

{- Doesn't know about keys that were just added with addDb. -}
inDb :: FsckHandle -> Key -> IO Bool
inDb (FsckHandle h _) = H.queryDbQueue h . inDb'

inDb' :: Key -> SqlPersistM Bool
inDb' k = do
	r <- selectList [FsckedKey ==. k] []
	return $ not $ null r
