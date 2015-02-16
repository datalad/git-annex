{- Sqlite database used for incremental fsck. 
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Database.Fsck (
	newPass,
	openDb,
	H.commitDb,
	H.closeDb,
	H.DbHandle,
	addDb,
	inDb,
	FsckedId,
) where

import Database.Types
import qualified Database.Handle as H
import Locations
import Utility.Directory
import Annex
import Types.Key
import Annex.Perms

import Database.Persist.TH
import Database.Esqueleto hiding (Key)
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.IO.Class (liftIO)
import System.Directory

{- Each key stored in the database has already been fscked as part
 - of the latest incremental fsck pass. -}
share [mkPersist sqlSettings, mkMigrate "migrateFsck"] [persistLowerCase|
Fscked
  key SKey
  UniqueKey key
  deriving Show
|]

{- The database is removed when starting a new incremental fsck pass. -}
newPass :: Annex ()
newPass = liftIO. nukeFile =<< fromRepo gitAnnexFsckDb

{- Opens the database, creating it atomically if it doesn't exist yet. -}
openDb :: Annex H.DbHandle
openDb = do
	db <- fromRepo gitAnnexFsckDb
	unlessM (liftIO $ doesFileExist db) $ do
		let newdb = db ++ ".new"
		h <- liftIO $ H.openDb newdb
		void $ liftIO $ H.runDb h $
			runMigrationSilent migrateFsck
		liftIO $ H.closeDb h
		setAnnexFilePerm newdb
		liftIO $ renameFile newdb db
	liftIO $ H.openDb db

addDb :: H.DbHandle -> Key -> IO ()
addDb h = void . H.runDb h . insert . Fscked . toSKey

inDb :: H.DbHandle -> Key -> IO Bool
inDb h k = H.runDb h $ do
	r <- select $ from $ \r -> do
		where_ (r ^. FsckedKey ==. val (toSKey k))
		return (r ^. FsckedKey)
	return $ not $ null r
