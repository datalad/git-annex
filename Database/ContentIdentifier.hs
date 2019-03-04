{- Sqlite database of ContentIdentifiers imported from special remotes.
 -
 - This contains a mapping from ContentIdentifier to Key.
 - The reverse mapping from Key to ContentIdentifier is stored in the
 - git-annex branch, see Logs.ContentIdentifier.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Database.ContentIdentifier (
	ContentIdentifierHandle,
	openDb,
	closeDb,
	flushDbQueue,
	recordContentIdentifier,
	getContentIdentifiers,
	getContentIdentifierKeys,
	ContentIdentifiersId,
) where

import Database.Types
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Annex.Common hiding (delete)
import Types.Import

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH

data ContentIdentifierHandle = ContentIdentifierHandle H.DbQueue

share [mkPersist sqlSettings, mkMigrate "migrateContentIdentifier"] [persistLowerCase|
ContentIdentifiers
  remote UUID
  cid ContentIdentifier
  key SKey
  ContentIdentifiersIndexRemoteKey remote key
  ContentIdentifiersIndexRemoteCID remote cid
  UniqueRemoteCidKey remote cid key
|]

{- Opens the database, creating it if it doesn't exist yet.
 -
 - Only a single process should write to the database at a time, so guard
 - any writes with the gitAnnexContentIdentifierLock.
 -}
openDb :: Annex ContentIdentifierHandle
openDb = do
	dbdir <- fromRepo gitAnnexContentIdentifierDbDir
	let db = dbdir </> "db"
	unlessM (liftIO $ doesFileExist db) $ do
		initDb db $ void $
			runMigrationSilent migrateContentIdentifier
	h <- liftIO $ H.openDbQueue H.SingleWriter db "content_identifiers"
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
recordContentIdentifier :: ContentIdentifierHandle -> UUID -> ContentIdentifier -> Key -> IO ()
recordContentIdentifier h u cid k = queueDb h $ do
	void $ insertUnique $ ContentIdentifiers u cid (toSKey k)

getContentIdentifiers :: ContentIdentifierHandle -> UUID -> Key -> IO [ContentIdentifier]
getContentIdentifiers (ContentIdentifierHandle h) u k = H.queryDbQueue h $ do
	l <- selectList [ContentIdentifiersKey ==. toSKey k] []
	return $ map (contentIdentifiersCid . entityVal) l

getContentIdentifierKeys :: ContentIdentifierHandle -> UUID -> ContentIdentifier -> IO [Key]
getContentIdentifierKeys (ContentIdentifierHandle h) u cid = 
	H.queryDbQueue h $ do
		l <- selectList
			[ ContentIdentifiersCid ==. cid
			, ContentIdentifiersRemote ==. u
			] []
		return $ map (fromSKey . contentIdentifiersKey . entityVal) l
