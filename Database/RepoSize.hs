{- Sqlite database used to track the sizes of repositories.
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
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

module Database.RepoSize (
	RepoSizeHandle,
	getRepoSizeHandle,
	openDb,
	closeDb,
	isOpenDb,
	lockDbWhile,
	getRepoSizes,
	setRepoSizes,
	startingLiveSizeChange,
	successfullyFinishedLiveSizeChange,
	removeStaleLiveSizeChange,
	removeStaleLiveSizeChanges,
	recordedRepoOffsets,
	liveRepoOffsets,
	setSizeChanges,
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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Exception
import Control.Concurrent

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
  changeid SizeChangeUniqueId
  changepid SizeChangeProcessId
  change SizeChange
  UniqueLiveSizeChange repo key changeid changepid
-- A rolling total of size changes that were removed from LiveSizeChanges
-- upon successful completion.
SizeChanges
  repo UUID
  rollingtotal FileSize
  UniqueRepoRollingTotal repo
-- The most recent size changes that were removed from LiveSizeChanges
-- upon successful completion.
RecentChanges
  repo UUID
  key Key
  change SizeChange
  UniqueRecentChange repo key
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
openDb = lockDbWhile permerr $ do
	dbdir <- calcRepo' gitAnnexRepoSizeDbDir
	let db = dbdir P.</> "db"
	unlessM (liftIO $ R.doesPathExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateRepoSizes
	h <- liftIO $ H.openDb db "repo_sizes"
	mkhandle (Just h)
  where
	mkhandle mh = do
		livev <- liftIO $ newMVar Nothing
		return $ RepoSizeHandle mh livev
	
	-- If permissions don't allow opening the database,
	-- just don't use it. Since this database is just a cache
	-- of information available in the git-annex branch, the same
	-- information can be queried from the branch, though much less
	-- efficiently.
	permerr _e = mkhandle Nothing

-- When the repository cannot be written to, openDb returns a
-- RepoSizeHandle that is not actually open, all operations on it will do
-- nothing.
isOpenDb :: RepoSizeHandle -> Bool
isOpenDb (RepoSizeHandle (Just _) _) = True
isOpenDb (RepoSizeHandle Nothing _) = False

closeDb :: RepoSizeHandle -> Annex ()
closeDb (RepoSizeHandle (Just h) _) = liftIO $ H.closeDb h
closeDb (RepoSizeHandle Nothing _) = noop

-- This does not prevent another process that has already 
-- opened the db from changing it at the same time.
lockDbWhile :: (IOException -> Annex a) -> Annex a -> Annex a
lockDbWhile permerr a = do
	lck <- calcRepo' gitAnnexRepoSizeDbLock
	catchPermissionDenied permerr $ withExclusiveLock lck a

{- Gets the sizes of repositories as of a commit to the git-annex
 - branch. -}
getRepoSizes :: RepoSizeHandle -> IO (M.Map UUID RepoSize, Maybe Sha)
getRepoSizes (RepoSizeHandle (Just h) _) = H.queryDb h $ do
	sizemap <- M.fromList <$> getRepoSizes'
	annexbranchsha <- getAnnexBranchCommit
	return (sizemap, annexbranchsha)
getRepoSizes (RepoSizeHandle Nothing _) = return (mempty, Nothing)

getRepoSizes' :: SqlPersistM [(UUID, RepoSize)]
getRepoSizes' = map conv <$> selectList [] []
  where
	conv entity = 
		let RepoSizes u sz = entityVal entity
		in (u, RepoSize sz)

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
setRepoSizes (RepoSizeHandle (Just h) _) sizemap branchcommitsha = 
	H.commitDb h $ do
		l <- getRepoSizes'
		forM_ (map fst l) $ \u ->
			unless (M.member u sizemap) $
				unsetRepoSize u
		forM_ (M.toList sizemap) $
			uncurry setRepoSize
		clearRecentChanges
		recordAnnexBranchCommit branchcommitsha
setRepoSizes (RepoSizeHandle Nothing _) _ _ = noop

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

startingLiveSizeChange :: RepoSizeHandle -> UUID -> Key -> SizeChange -> SizeChangeId -> IO ()
startingLiveSizeChange (RepoSizeHandle (Just h) _) u k sc cid = 
	H.commitDb h $ void $ upsertBy
		(UniqueLiveSizeChange u k
			(sizeChangeUniqueId cid)
			(sizeChangeProcessId cid))
		(LiveSizeChanges u k
			(sizeChangeUniqueId cid)
			(sizeChangeProcessId cid)
			sc)
		[ LiveSizeChangesChange =. sc
		, LiveSizeChangesChangeid =. sizeChangeUniqueId cid
		, LiveSizeChangesChangepid =. sizeChangeProcessId cid
		]
startingLiveSizeChange (RepoSizeHandle Nothing _) _ _ _ _ = noop

{- A live size change has successfully finished.
 -
 - Update the rolling total, add as a recent change,
 - and remove the live change in the same transaction.
 -
 - But, it's possible that the same change has been done by two
 - different processes or threads. If there is a matching recent change,
 - then this one is redundant, so remove it without updating the rolling
 - total.
 -}
successfullyFinishedLiveSizeChange :: RepoSizeHandle -> UUID -> Key -> SizeChange -> SizeChangeId -> IO ()
successfullyFinishedLiveSizeChange (RepoSizeHandle (Just h) _) u k sc cid =
	H.commitDb h $ do
		getRecentChange u k >>= \case
			Just sc' | sc == sc' -> remove
			_ -> go
  where
	go = do
		rollingtotal <- getSizeChangeFor u
		setSizeChangeFor u (updateRollingTotal rollingtotal sc k)
		addRecentChange u k sc
		remove
	remove = removeLiveSizeChange u k sc cid
successfullyFinishedLiveSizeChange (RepoSizeHandle Nothing _) _ _ _ _ = noop

updateRollingTotal :: FileSize -> SizeChange -> Key -> FileSize
updateRollingTotal t sc k = case sc of
	AddingKey -> t + ksz
	RemovingKey -> t - ksz
  where
	ksz = fromMaybe 0 $ fromKey keySize k

removeStaleLiveSizeChange :: RepoSizeHandle -> UUID -> Key -> SizeChange -> SizeChangeId -> IO ()
removeStaleLiveSizeChange (RepoSizeHandle (Just h) _) u k sc cid = 
	H.commitDb h $ removeLiveSizeChange u k sc cid
removeStaleLiveSizeChange (RepoSizeHandle Nothing _) _ _ _ _ = noop

removeLiveSizeChange :: UUID -> Key -> SizeChange -> SizeChangeId -> SqlPersistM ()
removeLiveSizeChange u k sc cid = 
	deleteWhere 
		[ LiveSizeChangesRepo ==. u
		, LiveSizeChangesKey ==. k
		, LiveSizeChangesChangeid ==. sizeChangeUniqueId cid
		, LiveSizeChangesChangepid ==. sizeChangeProcessId cid
		, LiveSizeChangesChange ==. sc
		]

removeStaleLiveSizeChanges :: RepoSizeHandle -> [StaleSizeChanger] -> IO ()
removeStaleLiveSizeChanges (RepoSizeHandle (Just h) _) stale = do
	let stalepids = map staleSizeChangerProcessId stale
	H.commitDb h $ deleteWhere [ LiveSizeChangesChangepid <-. stalepids ]
removeStaleLiveSizeChanges (RepoSizeHandle Nothing _) _ = noop

getLiveSizeChangesMap :: SqlPersistM (M.Map UUID [(Key, (SizeChange, SizeChangeId))])
getLiveSizeChangesMap = M.fromListWith (++) . map conv <$> getLiveSizeChanges
  where
	conv (LiveSizeChanges u k cid pid sc) = (u, [(k, (sc, sid))])
	  where
		sid = SizeChangeId cid pid

getLiveSizeChangesList :: SqlPersistM [(UUID, Key, SizeChange)]
getLiveSizeChangesList = map conv <$> getLiveSizeChanges
  where
	conv (LiveSizeChanges u k _cid _pid sc) = (u, k, sc)

getLiveSizeChanges :: SqlPersistM [LiveSizeChanges]
getLiveSizeChanges = map entityVal <$> selectList [] []

getSizeChanges :: SqlPersistM (M.Map UUID FileSize)
getSizeChanges = M.fromList . map conv <$> selectList [] []
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

setSizeChanges :: RepoSizeHandle -> M.Map UUID FileSize -> IO ()
setSizeChanges (RepoSizeHandle (Just h) _) sizemap = 
	H.commitDb h $ forM_ (M.toList sizemap) $ uncurry setSizeChangeFor
setSizeChanges (RepoSizeHandle Nothing _) _ = noop

addRecentChange :: UUID -> Key -> SizeChange -> SqlPersistM ()
addRecentChange u k sc =
	void $ upsertBy
		(UniqueRecentChange u k)
		(RecentChanges u k sc)
		[RecentChangesChange =. sc]

getRecentChange :: UUID -> Key -> SqlPersistM (Maybe SizeChange)
getRecentChange u k = do
	l <- selectList
		[ RecentChangesRepo ==. u
		, RecentChangesKey ==. k
		] []
	return $ case l of
		(s:_) -> Just $ recentChangesChange $ entityVal s
		[] -> Nothing

getRecentChanges :: SqlPersistM [(UUID, Key, SizeChange)]
getRecentChanges = map conv <$> selectList [] []
  where
	conv entity = 
		let RecentChanges u k sc = entityVal entity
		in (u, k, sc)

{- Clears recent changes, except when there is a live change that is
 - redundant with a recent change. -}
clearRecentChanges :: SqlPersistM ()
clearRecentChanges = do
	live <- getLiveSizeChangesList
	if null live
		then deleteWhere ([] :: [Filter RecentChanges])
		else do
			let liveset = S.fromList live
			rcs <- getRecentChanges
			forM_ rcs $ \rc@(u, k, sc) ->
				when (S.notMember rc liveset) $
					deleteWhere
						[ RecentChangesRepo ==. u
						, RecentChangesKey ==. k
						, RecentChangesChange ==. sc
						]

{- Gets the recorded offsets to sizes of Repos, not including live
 - changes. -}
recordedRepoOffsets :: RepoSizeHandle -> IO (M.Map UUID SizeOffset)
recordedRepoOffsets (RepoSizeHandle (Just h) _) = 
	M.map SizeOffset <$> H.queryDb h getSizeChanges
recordedRepoOffsets (RepoSizeHandle Nothing _) = pure mempty

{- Gets the offsets to sizes of Repos, including all live changes that
 - are happening now whose SizeChange matches the provided function.
 -
 - This does not necessarily include all changes that have been made,
 - only ones that had startingLiveSizeChange called for them will be
 - included.
 -
 - In the unlikely case where two live changes are occurring, one
 - adding a key and the other removing the same key, the one
 - adding the key is used, in order to err on the side of a larger
 - repository size.
 -
 - In the case where the same live change is recorded by two different
 - processes or threads, the first to complete will record it as a recent
 - change. This omits live changes that are redundant due to a recent
 - change already being recorded for the same change.
 - 
 - This is only expensive when there are a lot of live changes happening at
 - the same time.
 -}
liveRepoOffsets :: RepoSizeHandle -> (SizeChange -> Bool) -> IO (M.Map UUID SizeOffset)
liveRepoOffsets (RepoSizeHandle (Just h) _) wantedsizechange = H.queryDb h $ do
	sizechanges <- getSizeChanges
	livechanges <- getLiveSizeChangesMap
	let us = S.toList $ S.fromList $
		M.keys sizechanges ++ M.keys livechanges
	M.fromList <$> forM us (go sizechanges livechanges)
  where
	go sizechanges livechanges u = do
		let livechangesbykey = 
			M.fromListWith (++) $
				map (\(k, v) -> (k, [v])) $
					fromMaybe [] $
						M.lookup u livechanges
		-- This could be optimised to a single SQL join, rather
		-- than querying once for each live change. That would make
		-- it less expensive when there are a lot happening at the
		-- same time. Persistent is not capable of that join,
		-- it would need a dependency on esquelito.
		livechanges' <- combinelikelivechanges <$> 
			filterM (nonredundantlivechange livechangesbykey u)
				(fromMaybe [] $ M.lookup u livechanges)
		let sizechange = foldl' 
			(\t (k, sc) -> if wantedsizechange sc then updateRollingTotal t sc k else t)
			(fromMaybe 0 (M.lookup u sizechanges))
			livechanges'
		return (u, SizeOffset sizechange)
	
	combinelikelivechanges = 
		S.elems
			. S.fromList 
			. map (\(k, (sc, _)) -> (k, sc))

	nonredundantlivechange livechangesbykey u (k, (sc, cid))
		| null (competinglivechanges livechangesbykey k sc cid) =
			getRecentChange u k >>= pure . \case
				Nothing -> True
				Just sc' -> sc /= sc'
		| otherwise = pure False
	
	competinglivechanges livechangesbykey k RemovingKey cid =
		filter (\(sc', cid') -> cid /= cid' && sc' == AddingKey)
			(fromMaybe [] $ M.lookup k livechangesbykey)
	competinglivechanges _ _ AddingKey _ = []
liveRepoOffsets (RepoSizeHandle Nothing _) _ = pure mempty
