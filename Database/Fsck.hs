{- Sqlite database used for incremental fsck. 
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

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
import Annex.Locations
import Utility.PosixFiles
import Utility.Exception
import Annex.Common
import Annex.Perms
import Annex.LockFile

import Database.Persist.TH
import Database.Esqueleto hiding (Key)
import Data.Time.Clock

data FsckHandle = FsckHandle H.DbQueue UUID

{- Each key stored in the database has already been fscked as part
 - of the latest incremental fsck pass. -}
share [mkPersist sqlSettings, mkMigrate "migrateFsck"] [persistLowerCase|
Fscked
  key SKey
  UniqueKey key
|]

{- The database is removed when starting a new incremental fsck pass.
 -
 - This may fail, if other fsck processes are currently running using the
 - database. Removing the database in that situation would lead to crashes
 - or unknown behavior.
 -}
newPass :: UUID -> Annex Bool
newPass u = isJust <$> tryExclusiveLock (gitAnnexFsckDbLock u) go
  where
	go = liftIO . void . tryIO . removeDirectoryRecursive
		=<< fromRepo (gitAnnexFsckDbDir u)

{- Opens the database, creating it if it doesn't exist yet. -}
openDb :: UUID -> Annex FsckHandle
openDb u = do
	dbdir <- fromRepo (gitAnnexFsckDbDir u)
	let db = dbdir </> "db"
	unlessM (liftIO $ doesFileExist db) $ do
		let tmpdbdir = dbdir ++ ".tmp"
		let tmpdb = tmpdbdir </> "db"
		liftIO $ do
			createDirectoryIfMissing True tmpdbdir
			H.initDb tmpdb $ void $
				runMigrationSilent migrateFsck
		setAnnexDirPerm tmpdbdir
		setAnnexFilePerm tmpdb
		liftIO $ do
			void $ tryIO $ removeDirectoryRecursive dbdir
			rename tmpdbdir dbdir
	lockFileCached =<< fromRepo (gitAnnexFsckDbLock u)
	h <- liftIO $ H.openDbQueue db "fscked"
	return $ FsckHandle h u

closeDb :: FsckHandle -> Annex ()
closeDb (FsckHandle h u) = do
	liftIO $ H.closeDbQueue h
	unlockFile =<< fromRepo (gitAnnexFsckDbLock u)

addDb :: FsckHandle -> Key -> IO ()
addDb (FsckHandle h _) k = H.queueDb h checkcommit $ 
	void $ insertUnique $ Fscked sk
  where
	sk = toSKey k

	-- commit queue after 1000 files or 5 minutes, whichever comes first
	checkcommit sz lastcommittime
		| sz > 1000 = return True
		| otherwise = do
			now <- getCurrentTime
			return $ diffUTCTime lastcommittime now > 300

{- Doesn't know about keys that were just added with addDb. -}
inDb :: FsckHandle -> Key -> IO Bool
inDb (FsckHandle h _) = H.queryDbQueue h . inDb' . toSKey

inDb' :: SKey -> SqlPersistM Bool
inDb' sk = do
	r <- select $ from $ \r -> do
		where_ (r ^. FsckedKey ==. val sk)
		return (r ^. FsckedKey)
	return $ not $ null r
