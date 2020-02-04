{- Sqlite database of information about Keys
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Keys.SQL where

import Database.Types
import Database.Handle
import qualified Database.Queue as H
import Utility.InodeCache
import Utility.FileSystemEncoding
import Git.FilePath

import Database.Persist.Sql
import Database.Persist.TH
import Data.Time.Clock
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Conduit.List as CL

share [mkPersist sqlSettings, mkMigrate "migrateKeysDb"] [persistLowerCase|
Associated
  key IKey
  file SFilePath
  KeyFileIndex key file
  FileKeyIndex file key
Content
  key IKey
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
			return $ diffUTCTime now lastcommittime > 300

addAssociatedFile :: IKey -> TopFilePath -> WriteHandle -> IO ()
addAssociatedFile ik f = queueDb $ do
	-- If the same file was associated with a different key before,
	-- remove that.
	deleteWhere [AssociatedFile ==. af, AssociatedKey !=. ik]
	void $ insertUnique $ Associated ik af
  where
	af = toSFilePath (fromRawFilePath (getTopFilePath f))

-- Does not remove any old association for a file, but less expensive
-- than addAssociatedFile. Calling dropAllAssociatedFiles first and then
-- this is an efficient way to update all associated files.
addAssociatedFileFast :: IKey -> TopFilePath -> WriteHandle -> IO ()
addAssociatedFileFast ik f = queueDb $ void $ insertUnique $ Associated ik af
  where
	af = toSFilePath (fromRawFilePath (getTopFilePath f))

dropAllAssociatedFiles :: WriteHandle -> IO ()
dropAllAssociatedFiles = queueDb $
	deleteWhere ([] :: [Filter Associated])

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: IKey -> ReadHandle -> IO [TopFilePath]
getAssociatedFiles ik = readDb $ do
	l <- selectList [AssociatedKey ==. ik] []
	return $ map (asTopFilePath . toRawFilePath . fromSFilePath . associatedFile . entityVal) l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> ReadHandle -> IO [IKey]
getAssociatedKey f = readDb $ do
	l <- selectList [AssociatedFile ==. af] []
	return $ map (associatedKey . entityVal) l
  where
	af = toSFilePath (fromRawFilePath (getTopFilePath f))

removeAssociatedFile :: IKey -> TopFilePath -> WriteHandle -> IO ()
removeAssociatedFile ik f = queueDb $
	deleteWhere [AssociatedKey ==. ik, AssociatedFile ==. af]
  where
	af = toSFilePath (fromRawFilePath (getTopFilePath f))

addInodeCaches :: IKey -> [InodeCache] -> WriteHandle -> IO ()
addInodeCaches ik is = queueDb $
	forM_ is $ \i -> insertUnique $ Content ik (toSInodeCache i)

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: IKey -> ReadHandle -> IO [InodeCache]
getInodeCaches ik = readDb $ do
	l <- selectList [ContentKey ==. ik] []
	return $ map (fromSInodeCache . contentCache . entityVal) l

removeInodeCaches :: IKey -> WriteHandle -> IO ()
removeInodeCaches ik = queueDb $
	deleteWhere [ContentKey ==. ik]

{- Check if the inode is known to be used for an annexed file.
 -
 - This is currently slow due to the lack of indexes.
 -}
isInodeKnown :: InodeCache -> SentinalStatus -> ReadHandle -> IO Bool
isInodeKnown i s = readDb query
  where
	query 
		| sentinalInodesChanged s =
			withRawQuery likesql [] $ isJust <$> CL.head
		| otherwise =
			isJust <$> selectFirst [ContentCache ==. si] []
	
	si = toSInodeCache i
			
	likesql = T.concat
		[ "SELECT key FROM content WHERE "
		, T.intercalate " OR " $ map mklike (likeInodeCacheWeak i)
		, " LIMIT 1"
		]

	mklike p = T.concat
		[ "cache LIKE "
		, "'I \"" -- SInodeCache serializes as I "..."
		, T.pack p
		, "\"'"
		]
