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
	setRepoSizes,
) where

import Annex.Common
import Annex.LockFile
import Types.RepoSize
import Git.Types
import qualified Database.Queue as H
import Database.Init
import Database.Utility
import Database.Types
import qualified Utility.RawFilePath as R

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import qualified System.FilePath.ByteString as P
import qualified Data.Map as M

newtype RepoSizeHandle = RepoSizeHandle (Maybe H.DbQueue)

share [mkPersist sqlSettings, mkMigrate "migrateRepoSizes"] [persistLowerCase|
-- Corresponds to location log information from the git-annex branch.
RepoSizes
  repo UUID
  size Integer
  UniqueRepo repo
-- The last git-annex branch commit that was used to update RepoSizes.
AnnexBranch
  commit SSha
  UniqueCommit commit
|]

{- Opens the database, creating it if it doesn't exist yet.
 -
 - Multiple readers and writers can have the database open at the same
 - time. Database.Handle deals with the concurrency issues.
 - The lock is held while opening the database, so that when
 - the database doesn't exist yet, one caller wins the lock and
 - can create it undisturbed.
 -}
openDb :: Annex RepoSizeHandle
openDb = do
	lck <- calcRepo' gitAnnexRepoSizeDbLock
	catchPermissionDenied permerr $ withExclusiveLock lck $ do
		dbdir <- calcRepo' gitAnnexRepoSizeDbDir
		let db = dbdir P.</> "db"
		unlessM (liftIO $ R.doesPathExist db) $ do
			initDb db $ void $
				runMigrationSilent migrateRepoSizes
		h <- liftIO $ H.openDbQueue db "repo_sizes"
		return $ RepoSizeHandle (Just h)
  where
	-- If permissions don't allow opening the database,
	-- just don't use it. Since this database is just a cache
	-- of information available in the git-annex branch, the same
	-- information can be queried from the branch, though much less
	-- efficiently.
	permerr _e = return (RepoSizeHandle Nothing)

closeDb :: RepoSizeHandle -> Annex ()
closeDb (RepoSizeHandle (Just h)) = liftIO $ H.closeDbQueue h
closeDb (RepoSizeHandle Nothing) = noop

getRepoSizes :: RepoSizeHandle -> IO (M.Map UUID RepoSize, Maybe Sha)
getRepoSizes (RepoSizeHandle (Just h)) = H.queryDbQueue h $ do
	sizemap <- M.fromList . map conv <$> getRepoSizes'
	annexbranchsha <- getAnnexBranchCommit
	return (sizemap, annexbranchsha)
  where
	conv entity = 
		let RepoSizes u sz = entityVal entity
		in (u, RepoSize sz)
getRepoSizes (RepoSizeHandle Nothing) = return (mempty, Nothing)

getRepoSizes' :: SqlPersistM [Entity RepoSizes]
getRepoSizes' = selectList [] []

getAnnexBranchCommit :: SqlPersistM (Maybe Sha)
getAnnexBranchCommit = do
	l <- selectList ([] :: [Filter AnnexBranch]) []
	case l of
		(s:[]) -> return $ Just $ fromSSha $
			annexBranchCommit $ entityVal s
		_ -> return Nothing

{- Updates the recorded sizes of all repositories.
 -
 - This can be called without locking since the update runs in a single
 - transaction.
 -
 - Any repositories that are not in the provided map, but do have a size
 - recorded in the database will have it cleared. This is unlikely to
 - happen, but ensures that the database is consistent.
 -}
setRepoSizes :: RepoSizeHandle -> M.Map UUID RepoSize -> Sha -> IO ()
setRepoSizes (RepoSizeHandle (Just h)) sizemap branchcommitsha = 
	H.queueDb h commitimmediately $ do
		l <- getRepoSizes' 
		forM_ (map entityVal l) $ \(RepoSizes u _) ->
			unless (M.member u sizemap) $
				unsetRepoSize u
		forM_ (M.toList sizemap) $
			uncurry setRepoSize
		recordAnnexBranchCommit branchcommitsha
  where
	commitimmediately _ _ = pure True
setRepoSizes (RepoSizeHandle Nothing) _ _ = noop

setRepoSize :: UUID -> RepoSize -> SqlPersistM ()
setRepoSize u (RepoSize sz) =
	void $ upsertBy
		(UniqueRepo u)
		(RepoSizes u sz)
		[RepoSizesSize =. sz]

unsetRepoSize :: UUID -> SqlPersistM ()
unsetRepoSize u = deleteWhere [RepoSizesRepo ==. u]

recordAnnexBranchCommit :: Sha -> SqlPersistM ()
recordAnnexBranchCommit branchcommitsha = do
	deleteWhere ([] :: [Filter AnnexBranch])
	void $ insertUniqueFast $ AnnexBranch $ toSSha branchcommitsha
