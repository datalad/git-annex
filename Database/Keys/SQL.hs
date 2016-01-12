{- Sqlite database of information about Keys
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Database.Keys.SQL where

import Database.Types
import Database.Handle
import qualified Database.Queue as H
import Utility.InodeCache
import Git.FilePath

import Database.Persist.TH
import Database.Esqueleto hiding (Key)
import Data.Time.Clock
import Control.Monad

share [mkPersist sqlSettings, mkMigrate "migrateKeysDb"] [persistLowerCase|
Associated
  key SKey
  file FilePath
  KeyFileIndex key file
Content
  key SKey
  cache SInodeCache
  KeyCacheIndex key cache
|]

containedTable :: TableName
containedTable = "content"

createTables :: SqlPersistM ()
createTables = void $ runMigrationSilent migrateKeysDb

newtype ReadHandle = ReadHandle H.DbQueue

readDb :: SqlPersistM a -> ReadHandle -> IO a
readDb a (ReadHandle h) = H.queryDbQueue h a

newtype WriteHandle = WriteHandle H.DbQueue

queueDb :: SqlPersistM () -> WriteHandle -> IO ()
queueDb a (WriteHandle h) = H.queueDb h checkcommit a
  where
	-- commit queue after 1000 changes or 5 minutes, whichever comes first
	checkcommit sz lastcommittime
		| sz > 1000 = return True
		| otherwise = do
			now <- getCurrentTime
			return $ diffUTCTime lastcommittime now > 300

addAssociatedFile :: SKey -> TopFilePath -> WriteHandle -> IO ()
addAssociatedFile sk f = queueDb $ do
	-- If the same file was associated with a different key before,
	-- remove that.
	delete $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val (getTopFilePath f) &&. not_ (r ^. AssociatedKey ==. val sk))
	void $ insertUnique $ Associated sk (getTopFilePath f)

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: SKey -> ReadHandle -> IO [TopFilePath]
getAssociatedFiles sk = readDb $ do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk)
		return (r ^. AssociatedFile)
	return $ map (asTopFilePath . unValue) l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> ReadHandle -> IO [SKey]
getAssociatedKey f = readDb $ do
	l <- select $ from $ \r -> do
		where_ (r ^. AssociatedFile ==. val (getTopFilePath f))
		return (r ^. AssociatedKey)
	return $ map unValue l

removeAssociatedFile :: SKey -> TopFilePath -> WriteHandle -> IO ()
removeAssociatedFile sk f = queueDb $ 
	delete $ from $ \r -> do
		where_ (r ^. AssociatedKey ==. val sk &&. r ^. AssociatedFile ==. val (getTopFilePath f))

addInodeCaches :: SKey -> [InodeCache] -> WriteHandle -> IO ()
addInodeCaches sk is = queueDb $
	forM_ is $ \i -> insertUnique $ Content sk (toSInodeCache i)

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: SKey -> ReadHandle -> IO [InodeCache]
getInodeCaches sk = readDb $ do
	l <- select $ from $ \r -> do
		where_ (r ^. ContentKey ==. val sk)
		return (r ^. ContentCache)
	return $ map (fromSInodeCache . unValue) l

removeInodeCaches :: SKey -> WriteHandle -> IO ()
removeInodeCaches sk = queueDb $ 
	delete $ from $ \r -> do
		where_ (r ^. ContentKey ==. val sk)
