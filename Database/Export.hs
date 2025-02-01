{- Sqlite database used for exports to special remotes.
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TypeOperators, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Export (
	ExportHandle,
	openDb,
	closeDb,
	writeLockDbWhile,
	flushDbQueue,
	addExportedLocation,
	removeExportedLocation,
	getExportedLocation,
	isExportDirectoryEmpty,
	getExportTreeCurrent,
	recordExportTreeCurrent,
	getExportTree,
	getExportTreeKey,
	addExportTree,
	removeExportTree,
	updateExportTree,
	updateExportTree',
	updateExportTreeFromLog,
	updateExportDb,
	ExportedId,
	ExportedDirectoryId,
	ExportTreeId,
	ExportTreeCurrentId,
	ExportUpdateResult(..),
	ExportDiffUpdater,
	runExportDiffUpdater,
) where

import Database.Types
import qualified Database.Queue as H
import Database.Init
import Database.Utility
import Annex.Locations
import Annex.Common hiding (delete)
import Types.Export
import Annex.Export
import qualified Logs.Export as Log
import Annex.LockFile
import Annex.LockPool
import Git.Types
import Git.Sha
import Git.FilePath
import qualified Git.DiffTree

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH

data ExportHandle = ExportHandle H.DbQueue UUID

share [mkPersist sqlSettings, mkMigrate "migrateExport"] [persistLowerCase|
-- Files that have been exported to the remote and are present on it.
Exported
  key Key
  file SByteString
  ExportedIndex key file
-- Directories that exist on the remote, and the files that are in them.
ExportedDirectory
  subdir SByteString
  file SByteString
  ExportedDirectoryIndex subdir file
-- The content of the tree that has been exported to the remote.
-- Not all of these files are necessarily present on the remote yet.
ExportTree
  key Key
  file SByteString
  ExportTreeKeyFileIndex key file
  ExportTreeFileKeyIndex file key
-- The tree stored in ExportTree
ExportTreeCurrent
  tree SSha
  UniqueTree tree
|]

{- Opens the database, creating it if it doesn't exist yet.
 -
 - Only a single process should write to the export at a time, so guard
 - any writes with the gitAnnexExportLock.
 -}
openDb :: UUID -> Annex ExportHandle
openDb u = do
	dbdir <- calcRepo' (gitAnnexExportDbDir u)
	let db = dbdir </> literalOsPath "db"
	unlessM (liftIO $ doesDirectoryExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateExport
	h <- liftIO $ H.openDbQueue db "exported"
	return $ ExportHandle h u

closeDb :: ExportHandle -> Annex ()
closeDb (ExportHandle h _) = liftIO $ H.closeDbQueue h

queueDb :: ExportHandle -> SqlPersistM () -> IO ()
queueDb (ExportHandle h _) = H.queueDb h checkcommit
  where
	-- commit queue after 1000 changes
	checkcommit sz _lastcommittime
		| sz > 1000 = return True
		| otherwise = return False

flushDbQueue :: ExportHandle -> IO ()
flushDbQueue (ExportHandle h _) = H.flushDbQueue h

recordExportTreeCurrent :: ExportHandle -> Sha -> IO ()
recordExportTreeCurrent h s = queueDb h $ do
	deleteWhere ([] :: [Filter ExportTreeCurrent])
	void $ insertUniqueFast $ ExportTreeCurrent $ toSSha s

getExportTreeCurrent :: ExportHandle -> IO (Maybe Sha)
getExportTreeCurrent (ExportHandle h _) = H.queryDbQueue h $ do
	l <- selectList ([] :: [Filter ExportTreeCurrent]) []
	case l of
		(s:[]) -> return $ Just $ fromSSha $
			exportTreeCurrentTree $ entityVal s
		_ -> return Nothing

addExportedLocation :: ExportHandle -> Key -> ExportLocation -> IO ()
addExportedLocation h k el = queueDb h $ do
	void $ insertUniqueFast $ Exported k ef
	let edirs = map
		(\ed -> ExportedDirectory (SByteString (fromOsPath (fromExportDirectory ed))) ef)
		(exportDirectories el)
	putMany edirs
  where
	ef = SByteString (fromOsPath (fromExportLocation el))

removeExportedLocation :: ExportHandle -> Key -> ExportLocation -> IO ()
removeExportedLocation h k el = queueDb h $ do
	deleteWhere [ExportedKey ==. k, ExportedFile ==. ef]
	let subdirs = map
		(SByteString . fromOsPath . fromExportDirectory)
		(exportDirectories el)
	deleteWhere [ExportedDirectoryFile ==. ef, ExportedDirectorySubdir <-. subdirs]
  where
	ef = SByteString (fromOsPath (fromExportLocation el))

{- Note that this does not see recently queued changes. -}
getExportedLocation :: ExportHandle -> Key -> IO [ExportLocation]
getExportedLocation (ExportHandle h _) k = H.queryDbQueue h $ do
	l <- selectList [ExportedKey ==. k] []
	return $ map (mkExportLocation . (\(SByteString f) -> toOsPath f) . exportedFile . entityVal) l

{- Note that this does not see recently queued changes. -}
isExportDirectoryEmpty :: ExportHandle -> ExportDirectory -> IO Bool
isExportDirectoryEmpty (ExportHandle h _) d = H.queryDbQueue h $ do
	l <- selectList [ExportedDirectorySubdir ==. ed] []
	return $ null l
  where
	ed = SByteString $ fromOsPath $ fromExportDirectory d

{- Get locations in the export that might contain a key. -}
getExportTree :: ExportHandle -> Key -> IO [ExportLocation]
getExportTree (ExportHandle h _) k = H.queryDbQueue h $ do
	l <- selectList [ExportTreeKey ==. k] []
	return $ map (mkExportLocation . (\(SByteString f) -> toOsPath f) . exportTreeFile . entityVal) l

{- Get keys that might be currently exported to a location.
 -
 - Note that this does not see recently queued changes.
 -}
getExportTreeKey :: ExportHandle -> ExportLocation -> IO [Key]
getExportTreeKey (ExportHandle h _) el = H.queryDbQueue h $ do
	map (exportTreeKey . entityVal) 
		<$> selectList [ExportTreeFile ==. ef] []
  where
	ef = SByteString (fromOsPath (fromExportLocation el))

addExportTree :: ExportHandle -> Key -> ExportLocation -> IO ()
addExportTree h k loc = queueDb h $
	void $ insertUniqueFast $ ExportTree k ef
  where
	ef = SByteString (fromOsPath (fromExportLocation loc))

removeExportTree :: ExportHandle -> Key -> ExportLocation -> IO ()
removeExportTree h k loc = queueDb h $
	deleteWhere [ExportTreeKey ==. k, ExportTreeFile ==. ef]
  where
	ef = SByteString (fromOsPath (fromExportLocation loc))

-- An action that is passed the old and new values that were exported,
-- and updates state.
type ExportDiffUpdater
	= ExportHandle
	-> Maybe Key
	-- ^ old exported key
	-> Maybe Key
	-- ^ new exported key
	-> Git.DiffTree.DiffTreeItem
	-> Annex ()

mkExportDiffUpdater
	:: (ExportHandle -> Key -> ExportLocation -> IO ())
	-> (ExportHandle -> Key -> ExportLocation -> IO ())
	-> ExportDiffUpdater
mkExportDiffUpdater removeold addnew h srcek dstek i = do
	case srcek of
		Nothing -> return ()
		Just k -> liftIO $ removeold h k loc
	case dstek of
		Nothing -> return ()
		Just k -> liftIO $ addnew h k loc
  where
	loc = mkExportLocation $ getTopFilePath $ Git.DiffTree.file i

runExportDiffUpdater :: ExportDiffUpdater -> ExportHandle -> Sha -> Sha -> Annex ()
runExportDiffUpdater updater h old new = do
	(diff, cleanup) <- inRepo $
		Git.DiffTree.diffTreeRecursive old new
	forM_ diff $ \i -> do
		srcek <- getek (Git.DiffTree.srcsha i)
		dstek <- getek (Git.DiffTree.dstsha i)
		updater h srcek dstek i
	void $ liftIO cleanup
  where
	getek sha
		| sha `elem` nullShas = return Nothing
		| otherwise = Just <$> exportKey sha

{- Diff from the old to the new tree and update the ExportTree table. -}
updateExportTree :: ExportHandle -> Sha -> Sha -> Annex ()
updateExportTree = runExportDiffUpdater updateExportTree'

updateExportTree' :: ExportDiffUpdater
updateExportTree' = mkExportDiffUpdater removeExportTree addExportTree

{- Diff from the old to the new tree and update all tables in the export
 - database. Should only be used when all the files in the new tree have
 - been verified to already be present in the export remote. -}
updateExportDb :: ExportHandle -> Sha -> Sha -> Annex ()
updateExportDb = runExportDiffUpdater $ mkExportDiffUpdater removeold addnew
  where
	removeold h k loc = liftIO $ do
		removeExportTree h k loc
		removeExportedLocation h k loc
	addnew h k loc = liftIO $ do
		addExportTree h k loc
		addExportedLocation h k loc

{- Runs an action with the database locked for write. Waits for any other
 - writers to finish first. The queue is flushed at the end.
 -
 - This first updates the ExportTree table with any new information 
 - from the git-annex branch export log.
 -}
writeLockDbWhile :: ExportHandle -> Annex a -> Annex a
writeLockDbWhile db@(ExportHandle _ u) a = do
	updatelck <- takeExclusiveLock =<< calcRepo' (gitAnnexExportUpdateLock u)
	exlck <- calcRepo' (gitAnnexExportLock u)
	withExclusiveLock exlck $ do
		bracket_ (setup updatelck) cleanup a
  where
	setup updatelck = do
		void $ updateExportTreeFromLog' db
		-- flush the update so it's available immediately to
		-- anything waiting on the updatelck
		liftIO $ flushDbQueue db
		liftIO $ dropLock updatelck
	cleanup = liftIO $ flushDbQueue db

data ExportUpdateResult = ExportUpdateSuccess | ExportUpdateConflict
	deriving (Eq)

{- Updates the ExportTree table with information from the
 - git-annex branch export log.
 -
 - This can safely be called whether the database is locked for write or
 - not. Either way, it will block until the update is complete.
 -}
updateExportTreeFromLog :: ExportHandle -> Annex ExportUpdateResult
updateExportTreeFromLog db@(ExportHandle _ u) = do
	-- If another process or thread is performing the update,
	-- this will block until it's done.
	exlck <- calcRepo' (gitAnnexExportUpdateLock u)
	withExclusiveLock exlck $ do
		lck <- calcRepo' (gitAnnexExportLock u)
		-- If the database is locked by something else,
		-- this will not run the update. But, in that case,
		-- writeLockDbWhile is running, and has already
		-- completed the update, so we don't need to do anything.
		mr <- tryExclusiveLock lck $
			updateExportTreeFromLog' db
		case mr of
			Just r -> return r
			Nothing -> do
				old <- liftIO $ fromMaybe emptyTree
					<$> getExportTreeCurrent db
				l <- Log.getExport u
				return $ case Log.exportedTreeishes l of
					[] -> ExportUpdateSuccess
					(new:[]) 
						| new /= old -> ExportUpdateSuccess
						| new == old -> ExportUpdateSuccess
					_ts -> ExportUpdateConflict

{- The database should be locked when calling this. -}
updateExportTreeFromLog' :: ExportHandle -> Annex ExportUpdateResult
updateExportTreeFromLog' db@(ExportHandle _ u) = do
	old <- liftIO $ fromMaybe emptyTree
		<$> getExportTreeCurrent db
	l <- Log.getExport u
	case Log.exportedTreeishes l of
		[] -> return ExportUpdateSuccess
		(new:[]) 
			| new /= old -> do
				updateExportTree db old new
				liftIO $ recordExportTreeCurrent db new
				liftIO $ flushDbQueue db
				return ExportUpdateSuccess
			| new == old -> return ExportUpdateSuccess
		_ts -> return ExportUpdateConflict
