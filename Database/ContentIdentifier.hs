{- Sqlite database of ContentIdentifiers imported from special remotes.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if MIN_VERSION_persistent_template(2,8,0)
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module Database.ContentIdentifier (
	ContentIdentifierHandle,
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
import Annex.Locations
import Annex.Common hiding (delete)
import qualified Annex.Branch
import Types.Import
import Types.RemoteState
import Git.Types
import Git.Sha
import Git.FilePath
import qualified Git.Ref
import qualified Git.DiffTree as DiffTree
import Logs
import qualified Logs.ContentIdentifier as Log
import qualified Utility.RawFilePath as R

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import Database.Persist.Sqlite (runSqlite)
import qualified System.FilePath.ByteString as P
import qualified Data.Text as T

data ContentIdentifierHandle = ContentIdentifierHandle H.DbQueue

share [mkPersist sqlSettings, mkMigrate "migrateContentIdentifier"] [persistLowerCase|
ContentIdentifiers
  remote UUID
  cid ContentIdentifier
  key Key
  ContentIndentifiersKeyRemoteCidIndex key remote cid
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
	let db = dbdir P.</> "db"
	ifM (liftIO $ not <$> R.doesPathExist db)
		( initDb db $ void $ 
			runMigrationSilent migrateContentIdentifier
		-- Migrate from old version of database, which had
		-- an incorrect uniqueness constraint on the
		-- ContentIdentifiers table.
		, liftIO $ runSqlite (T.pack (fromRawFilePath db)) $ void $
			runMigrationSilent migrateContentIdentifier
		)
	h <- liftIO $ H.openDbQueue db "content_identifiers"
	return $ ContentIdentifierHandle h

closeDb :: ContentIdentifierHandle -> Annex ()
closeDb (ContentIdentifierHandle h) = liftIO $ H.closeDbQueue h

queueDb :: ContentIdentifierHandle -> SqlPersistM () -> IO ()
queueDb (ContentIdentifierHandle h) = H.queueDb h checkcommit
  where
	-- commit queue after 1000 changes
	checkcommit sz _lastcommittime
		| sz > 1000 = return True
		| otherwise = return False

flushDbQueue :: ContentIdentifierHandle -> IO ()
flushDbQueue (ContentIdentifierHandle h) = H.flushDbQueue h

-- Be sure to also update the git-annex branch when using this.
recordContentIdentifier :: ContentIdentifierHandle -> RemoteStateHandle -> ContentIdentifier -> Key -> IO ()
recordContentIdentifier h (RemoteStateHandle u) cid k = queueDb h $ do
	void $ insertUnique $ ContentIdentifiers u cid k

getContentIdentifiers :: ContentIdentifierHandle -> RemoteStateHandle -> Key -> IO [ContentIdentifier]
getContentIdentifiers (ContentIdentifierHandle h) (RemoteStateHandle u) k = 
	H.queryDbQueue h $ do
		l <- selectList
			[ ContentIdentifiersKey ==. k
			, ContentIdentifiersRemote ==. u
			] []
		return $ map (contentIdentifiersCid . entityVal) l

getContentIdentifierKeys :: ContentIdentifierHandle -> RemoteStateHandle -> ContentIdentifier -> IO [Key]
getContentIdentifierKeys (ContentIdentifierHandle h) (RemoteStateHandle u) cid = 
	H.queryDbQueue h $ do
		l <- selectList
			[ ContentIdentifiersCid ==. cid
			, ContentIdentifiersRemote ==. u
			] []
		return $ map (contentIdentifiersKey . entityVal) l

recordAnnexBranchTree :: ContentIdentifierHandle -> Sha -> IO ()
recordAnnexBranchTree h s = queueDb h $ do
        deleteWhere ([] :: [Filter AnnexBranch])
        void $ insertUnique $ AnnexBranch $ toSSha s

getAnnexBranchTree :: ContentIdentifierHandle -> IO Sha
getAnnexBranchTree (ContentIdentifierHandle h) = H.queryDbQueue h $ do
        l <- selectList ([] :: [Filter AnnexBranch]) []
        case l of
                (s:[]) -> return $ fromSSha $ annexBranchTree $ entityVal s
                _ -> return emptyTree

{- Check if the git-annex branch has been updated and the database needs
 - to be updated with any new content identifiers in it. -}
needsUpdateFromLog :: ContentIdentifierHandle -> Annex (Maybe (Sha, Sha))
needsUpdateFromLog db = do
	oldtree <- liftIO $ getAnnexBranchTree db
	inRepo (Git.Ref.tree Annex.Branch.fullname) >>= \case
		Just currtree | currtree /= oldtree ->
			return $ Just (oldtree, currtree)
		_ -> return Nothing

{- The database should be locked for write when calling this. -}
updateFromLog :: ContentIdentifierHandle -> (Sha, Sha) -> Annex ()
updateFromLog db (oldtree, currtree) = do
	(l, cleanup) <- inRepo $
		DiffTree.diffTreeRecursive oldtree currtree
	mapM_ go l
	void $ liftIO $ cleanup
	liftIO $ do
		recordAnnexBranchTree db currtree
		flushDbQueue db
  where
	go ti = case extLogFileKey remoteContentIdentifierExt (getTopFilePath (DiffTree.file ti)) of
		Nothing -> return ()
		Just k -> do
			l <- Log.getContentIdentifiers k
			liftIO $ forM_ l $ \(rs, cids) ->
				forM_ cids $ \cid ->
					recordContentIdentifier db rs cid k
