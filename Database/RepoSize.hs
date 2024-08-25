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
	getRepoSizeHandle,
	openDb,
	closeDb,
	getRepoSizes,
	setRepoSizes,
	getLiveSizeChanges,
	startingLiveSizeChange,
	successfullyFinishedLiveSizeChange,
	staleLiveSizeChange,
	getSizeChanges,
) where

import Annex.Common
import qualified Annex
import Database.RepoSize.Handle
import qualified Database.Handle as H
import Database.Init
import Database.Utility
import Database.Types
import Annex.LockFile
import Git.Types
import qualified Utility.RawFilePath as R

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import qualified System.FilePath.ByteString as P
import qualified Data.Map as M

share [mkPersist sqlSettings, mkMigrate "migrateRepoSizes"] [persistLowerCase|
-- Corresponds to location log information from the git-annex branch.
RepoSizes
  repo UUID
  size FileSize
  UniqueRepo repo
-- The last git-annex branch commit that was used to update RepoSizes.
AnnexBranch
  commit SSha
  UniqueCommit commit
-- Changes that are currently being made that affect repo sizes.
-- (Only updated when preferred content expressions are in use that need
-- live size changes.)
LiveSizeChanges
  repo UUID
  key Key
  changeid SizeChangeId
  change SizeChange
  UniqueLiveSizeChange repo key changeid
-- A rolling total of size changes that were removed from LiveSizeChanges
-- upon successful completion.
SizeChanges
  repo UUID
  rollingtotal FileSize
  UniqueRepoRollingTotal repo
|]

{- Gets a handle to the database. It's cached in Annex state. -}
getRepoSizeHandle :: Annex RepoSizeHandle
getRepoSizeHandle = Annex.getState Annex.reposizehandle >>= \case
	Just h -> return h
	Nothing -> do
		h <- openDb
		Annex.changeState $ \s -> s { Annex.reposizehandle = Just h }
		return h

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
		h <- liftIO $ H.openDb db "repo_sizes"
		return $ RepoSizeHandle (Just h)
  where
	-- If permissions don't allow opening the database,
	-- just don't use it. Since this database is just a cache
	-- of information available in the git-annex branch, the same
	-- information can be queried from the branch, though much less
	-- efficiently.
	permerr _e = return (RepoSizeHandle Nothing)

closeDb :: RepoSizeHandle -> Annex ()
closeDb (RepoSizeHandle (Just h)) = liftIO $ H.closeDb h
closeDb (RepoSizeHandle Nothing) = noop

getRepoSizes :: RepoSizeHandle -> IO (M.Map UUID RepoSize, Maybe Sha)
getRepoSizes (RepoSizeHandle (Just h)) = H.queryDb h $ do
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
	H.commitDb h $ do
		l <- getRepoSizes' 
		forM_ (map entityVal l) $ \(RepoSizes u _) ->
			unless (M.member u sizemap) $
				unsetRepoSize u
		forM_ (M.toList sizemap) $
			uncurry setRepoSize
		recordAnnexBranchCommit branchcommitsha
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

{- If there is already a size change for the same UUID, Key,
 - and SizeChangeId, it is overwritten with the new size change. -}
startingLiveSizeChange :: RepoSizeHandle -> UUID -> Key -> SizeChange -> SizeChangeId -> IO ()
startingLiveSizeChange (RepoSizeHandle (Just h)) u k sc sid = 
	H.commitDb h $ void $ upsertBy
		(UniqueLiveSizeChange u k sid)
		(LiveSizeChanges u k sid sc)
		[ LiveSizeChangesChange =. sc
		, LiveSizeChangesChangeid =. sid
		]
startingLiveSizeChange (RepoSizeHandle Nothing) _ _ _ _ = noop

successfullyFinishedLiveSizeChange :: RepoSizeHandle -> UUID -> Key -> SizeChange -> SizeChangeId -> IO ()
successfullyFinishedLiveSizeChange (RepoSizeHandle (Just h)) u k sc sid =
	H.commitDb h $ do
		-- Update the rolling total and remove the live change in the
		-- same transaction.
		rollingtotal <- getSizeChangeFor u
		setSizeChangeFor u (updaterollingtotal rollingtotal)
		removeLiveSizeChange u k sc sid
  where
	updaterollingtotal t = case sc of
		AddingKey -> t + ksz
		RemovingKey -> t - ksz
	ksz = fromMaybe 0 $ fromKey keySize k
successfullyFinishedLiveSizeChange (RepoSizeHandle Nothing) _ _ _ _ = noop

staleLiveSizeChange :: RepoSizeHandle -> UUID -> Key -> SizeChange -> SizeChangeId -> IO ()
staleLiveSizeChange (RepoSizeHandle (Just h)) u k sc sid = 
	H.commitDb h $ removeLiveSizeChange u k sc sid
staleLiveSizeChange (RepoSizeHandle Nothing) _ _ _ _ = noop

removeLiveSizeChange :: UUID -> Key -> SizeChange -> SizeChangeId -> SqlPersistM ()
removeLiveSizeChange u k sc sid = 
	deleteWhere 
		[ LiveSizeChangesRepo ==. u
		, LiveSizeChangesKey ==. k
		, LiveSizeChangesChangeid ==. sid
		, LiveSizeChangesChange ==. sc
		]

getLiveSizeChanges :: RepoSizeHandle -> IO (M.Map UUID (Key, SizeChange, SizeChangeId))
getLiveSizeChanges (RepoSizeHandle (Just h)) = H.queryDb h $ do
	m <- M.fromList . map conv <$> getLiveSizeChanges'
	return m
  where
	conv entity = 
		let LiveSizeChanges u k sid sc = entityVal entity
		in (u, (k, sc, sid))
getLiveSizeChanges (RepoSizeHandle Nothing) = return mempty

getLiveSizeChanges' :: SqlPersistM [Entity LiveSizeChanges]
getLiveSizeChanges' = selectList [] []

getSizeChanges :: RepoSizeHandle -> IO (M.Map UUID FileSize)
getSizeChanges (RepoSizeHandle (Just h)) = H.queryDb h getSizeChanges'
getSizeChanges (RepoSizeHandle Nothing) = return mempty

getSizeChanges' :: SqlPersistM (M.Map UUID FileSize)
getSizeChanges' = M.fromList . map conv <$> selectList [] []
  where
	conv entity =
		let SizeChanges u n = entityVal entity
		in (u, n)

getSizeChangeFor :: UUID -> SqlPersistM FileSize
getSizeChangeFor u = do
	l <- selectList [SizeChangesRepo ==. u] []
	return $ case l of
		(s:_) -> sizeChangesRollingtotal $ entityVal s
		[] -> 0

setSizeChangeFor :: UUID -> FileSize -> SqlPersistM ()
setSizeChangeFor u sz = 
	void $ upsertBy
		(UniqueRepoRollingTotal u)
		(SizeChanges u sz)
		[SizeChangesRollingtotal =. sz]
