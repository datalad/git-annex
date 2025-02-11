{- Sqlite database used for incremental fsck. 
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import Database.Utility
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
newPass u = do
	lck <- calcRepo' (gitAnnexFsckDbLock u)
	isJust <$> tryExclusiveLock lck go
  where
	go = do
		removedb =<< calcRepo' (gitAnnexFsckDbDir u)
		removedb =<< calcRepo' (gitAnnexFsckDbDirOld u)
	removedb = liftIO . void . tryIO . removeDirectoryRecursive

{- Opens the database, creating it if it doesn't exist yet. -}
openDb :: UUID -> Annex FsckHandle
openDb u = do
	dbdir <- calcRepo' (gitAnnexFsckDbDir u)
	let db = dbdir </> literalOsPath "db"
	unlessM (liftIO $ doesFileExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateFsck
	lockFileCached =<< calcRepo' (gitAnnexFsckDbLock u)
	h <- liftIO $ H.openDbQueue db "fscked"
	return $ FsckHandle h u

closeDb :: FsckHandle -> Annex ()
closeDb (FsckHandle h u) = do
	liftIO $ H.closeDbQueue h
	unlockFile =<< calcRepo' (gitAnnexFsckDbLock u)

addDb :: FsckHandle -> Key -> IO ()
addDb (FsckHandle h _) k = H.queueDb h checkcommit $
	void $ insertUniqueFast $ Fscked k
  where
	-- Commit queue after 1000 changes or 5 minutes, whichever comes first.
	-- The time based commit allows for an incremental fsck to be
	-- interrupted and not lose much work.
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
