{- Sqlite database of ContentIdentifiers imported from special remotes.
 -
 - Copyright 2019-2023 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TypeOperators, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.ContentIdentifier (
	ContentIdentifierHandle,
	databaseIsEmpty,
	openDb,
	closeDb,
	flushDbQueue,
	recordContentIdentifier,
	getContentIdentifiers,
	getContentIdentifierKeys,
	recordAnnexBranchTree,
	getAnnexBranchTree,
	needsUpdateFromLog,
	updateFromLog,
	ContentIdentifiersId,
	AnnexBranchId,
) where

import Database.Types
import qualified Database.Queue as H
import Database.Init
import Database.Utility
import Annex.Locations
import Annex.Common hiding (delete)
import qualified Annex.Branch
import Types.Import
import Types.RemoteState
import Git.Types
import Git.Sha
import Git.FilePath
import qualified Git.DiffTree as DiffTree
import Logs
import qualified Logs.ContentIdentifier as Log

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import Database.RawFilePath

data ContentIdentifierHandle = ContentIdentifierHandle H.DbQueue Bool

databaseIsEmpty :: ContentIdentifierHandle -> Bool
databaseIsEmpty (ContentIdentifierHandle _ b) = b

-- Note on indexes: ContentIndentifiersKeyRemoteCidIndex etc are really
-- uniqueness constraints, which cause sqlite to automatically add indexes.
-- So when adding indexes, have to take care to only add ones that work as
-- uniqueness constraints. (Unfortunately persistent does not support indexes
-- that are not uniqueness constraints; 
-- https://github.com/yesodweb/persistent/issues/109)
-- 
-- ContentIndentifiersKeyRemoteCidIndex speeds up queries like 
-- getContentIdentifiers, but it is not used for
-- getContentIdentifierKeys. ContentIndentifiersCidRemoteKeyIndex was
-- addedto speed that up.
share [mkPersist sqlSettings, mkMigrate "migrateContentIdentifier"] [persistLowerCase|
ContentIdentifiers
  remote UUID
  cid ContentIdentifier
  key Key
  ContentIndentifiersKeyRemoteCidIndex key remote cid
  ContentIndentifiersCidRemoteKeyIndex cid remote key
-- The last git-annex branch tree sha that was used to update
-- ContentIdentifiers
AnnexBranch
  tree SSha
  UniqueTree tree
|]

{- Opens the database, creating it if it doesn't exist yet.
 -
 - Only a single process should write to the database at a time, so guard
 - any writes with the gitAnnexContentIdentifierLock.
 -}
openDb :: Annex ContentIdentifierHandle
openDb = do
	dbdir <- calcRepo' gitAnnexContentIdentifierDbDir
	let db = dbdir </> literalOsPath "db"
	isnew <- liftIO $ not <$> doesFileExist db
	if isnew
		then initDb db $ void $ 
			runMigrationSilent migrateContentIdentifier
		-- Migrate from old versions of database, which had buggy
		-- and suboptimal uniqueness constraints.
		else liftIO $ runSqlite' (fromOsPath db) $ void $
			runMigrationSilent migrateContentIdentifier
	h <- liftIO $ H.openDbQueue db "content_identifiers"
	return $ ContentIdentifierHandle h isnew

closeDb :: ContentIdentifierHandle -> Annex ()
closeDb (ContentIdentifierHandle h _) = liftIO $ H.closeDbQueue h

queueDb :: ContentIdentifierHandle -> SqlPersistM () -> IO ()
queueDb (ContentIdentifierHandle h _) = H.queueDb h checkcommit
  where
	-- commit queue after 1000 changes
	checkcommit sz _lastcommittime
		| sz > 1000 = return True
		| otherwise = return False

flushDbQueue :: ContentIdentifierHandle -> IO ()
flushDbQueue (ContentIdentifierHandle h _) = H.flushDbQueue h

-- Be sure to also update the git-annex branch when using this.
recordContentIdentifier :: ContentIdentifierHandle -> RemoteStateHandle -> ContentIdentifier -> Key -> IO ()
recordContentIdentifier h (RemoteStateHandle u) cid k = queueDb h $ do
	void $ insertUniqueFast $ ContentIdentifiers u cid k

getContentIdentifiers :: ContentIdentifierHandle -> RemoteStateHandle -> Key -> IO [ContentIdentifier]
getContentIdentifiers (ContentIdentifierHandle h _) (RemoteStateHandle u) k = 
	H.queryDbQueue h $ do
		l <- selectList
			[ ContentIdentifiersKey ==. k
			, ContentIdentifiersRemote ==. u
			] []
		return $ map (contentIdentifiersCid . entityVal) l

getContentIdentifierKeys :: ContentIdentifierHandle -> RemoteStateHandle -> ContentIdentifier -> IO [Key]
getContentIdentifierKeys (ContentIdentifierHandle h _) (RemoteStateHandle u) cid = 
	H.queryDbQueue h $ do
		l <- selectList
			[ ContentIdentifiersCid ==. cid
			, ContentIdentifiersRemote ==. u
			] []
		return $ map (contentIdentifiersKey . entityVal) l

recordAnnexBranchTree :: ContentIdentifierHandle -> Sha -> IO ()
recordAnnexBranchTree h s = queueDb h $ do
	deleteWhere ([] :: [Filter AnnexBranch])
	void $ insertUniqueFast $ AnnexBranch $ toSSha s

getAnnexBranchTree :: ContentIdentifierHandle -> IO Sha
getAnnexBranchTree (ContentIdentifierHandle h _) = H.queryDbQueue h $ do
	l <- selectList ([] :: [Filter AnnexBranch]) []
	case l of
		(s:[]) -> return $ fromSSha $ annexBranchTree $ entityVal s
		_ -> return emptyTree

{- Check if the git-annex branch has been updated and the database needs
 - to be updated with any new content identifiers in it. -}
needsUpdateFromLog :: ContentIdentifierHandle -> Annex (Maybe (Sha, Sha))
needsUpdateFromLog db = do
	oldtree <- liftIO $ getAnnexBranchTree db
	Annex.Branch.updatedFromTree oldtree

{- The database should be locked for write when calling this. -}
updateFromLog :: ContentIdentifierHandle -> (Sha, Sha) -> Annex ContentIdentifierHandle
updateFromLog db@(ContentIdentifierHandle h _) (oldtree, currtree) = do
	(l, cleanup) <- inRepo $
		DiffTree.diffTreeRecursive oldtree currtree
	mapM_ go l
	void $ liftIO $ cleanup
	liftIO $ do
		recordAnnexBranchTree db currtree
		flushDbQueue db
	return (ContentIdentifierHandle h False)
  where
	go ti = case extLogFileKey remoteContentIdentifierExt (getTopFilePath (DiffTree.file ti)) of
		Nothing -> return ()
		Just k -> do
			l <- Log.getContentIdentifiers k
			liftIO $ forM_ l $ \(rs, cids) ->
				forM_ cids $ \cid ->
					recordContentIdentifier db rs cid k
