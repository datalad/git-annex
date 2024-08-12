{- Sqlite database used to track the sizes of repositories.
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
#if MIN_VERSION_persistent_template(2,8,0)
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module Database.RepoSize (
	RepoSizeHandle,
	openDb,
	closeDb,
	getRepoSizes,	
	setRepoSize,
	updateRepoSize,
) where

import Types.RepoSize
import Database.Types ()
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Annex.Common
import qualified Utility.RawFilePath as R

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import qualified System.FilePath.ByteString as P
import qualified Data.Map as M

newtype RepoSizeHandle = RepoSizeHandle H.DbQueue

share [mkPersist sqlSettings, mkMigrate "migrateRepoSizes"] [persistLowerCase|
RepoSizes
  repo UUID
  size Integer
  UniqueRepo repo
|]

{- Opens the database, creating it if it doesn't exist yet.
 -
 - No locking is done by this, so caller must prevent multiple processes
 - running this at the same time.
 -}
openDb :: Annex RepoSizeHandle
openDb = do
	dbdir <- calcRepo' gitAnnexRepoSizeDbDir
	let db = dbdir P.</> "db"
	unlessM (liftIO $ R.doesPathExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateRepoSizes
	h <- liftIO $ H.openDbQueue db "reposizes"
	return $ RepoSizeHandle h

closeDb :: RepoSizeHandle -> Annex ()
closeDb (RepoSizeHandle h) = liftIO $ H.closeDbQueue h

{- Doesn't see changes that were just made with setRepoSize or
 - updateRepoSize before flushing the queue. -}
getRepoSizes :: RepoSizeHandle -> IO (M.Map UUID RepoSize)
getRepoSizes (RepoSizeHandle h) = H.queryDbQueue h $
	M.fromList . map conv <$> getRepoSizes'
  where
	conv entity = 
		let RepoSizes u sz = entityVal entity
		in (u, RepoSize sz)

getRepoSizes' :: SqlPersistM [Entity RepoSizes]
getRepoSizes' = selectList [] []

setRepoSize :: UUID -> RepoSize -> RepoSizeHandle -> IO ()
setRepoSize u (RepoSize sz) (RepoSizeHandle h) = H.queueDb h checkCommit $
	void $ upsertBy
		(UniqueRepo u)
		(RepoSizes u sz)
		[RepoSizesSize =. sz]

{- Applies an offset to the size. If no size is recorded for the repo, does
 - nothing. -}
updateRepoSize :: UUID -> Integer -> RepoSizeHandle -> IO ()
updateRepoSize u offset (RepoSizeHandle h) = H.queueDb h checkCommit $
	void $ updateWhere
		[RepoSizesRepo ==. u]
		[RepoSizesSize +=. offset]

checkCommit :: H.QueueSize -> H.LastCommitTime -> IO Bool
checkCommit sz _lastcommittime
	| sz > 1000 = return True
	| otherwise = return False
