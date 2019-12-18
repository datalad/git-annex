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

module Database.Keys.SQL where

import Database.Types
import Database.Handle
import qualified Database.Queue as H
import Utility.InodeCache
import Utility.FileSystemEncoding
import Git.FilePath

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import Data.Time.Clock
import Control.Monad
import Data.Maybe

-- Note on indexes: KeyFileIndex etc are really uniqueness constraints,
-- which cause sqlite to automatically add indexes. So when adding indexes,
-- have to take care to only add ones that work as uniqueness constraints.
-- (Unfortunatly persistent does not support indexes that are not
-- uniqueness constraints; https://github.com/yesodweb/persistent/issues/109)
--
-- KeyFileIndex contains both the key and the file because the combined
-- pair is unique, whereas the same key can appear in the table multiple
-- times with different files.
--
-- The other benefit to including the file in the index is that it makes
-- queries that include the file faster, since it's a covering index.
--
-- The KeyFileIndex only speeds up selects for a key, since it comes first.
-- To also speed up selects for a file, there's a separate FileKeyIndex.
share [mkPersist sqlSettings, mkMigrate "migrateKeysDb"] [persistLowerCase|
Associated
  key Key
  file SFilePath
  KeyFileIndex key file
  FileKeyIndex file key
Content
  key Key
  inodecache InodeCache
  filesize FileSize
  mtime EpochTime
  KeyInodeCacheIndex key inodecache
  InodeCacheKeyIndex inodecache key
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

addAssociatedFile :: Key -> TopFilePath -> WriteHandle -> IO ()
addAssociatedFile k f = queueDb $ do
	-- If the same file was associated with a different key before,
	-- remove that.
	deleteWhere [AssociatedFile ==. af, AssociatedKey !=. k]
	void $ insertUnique $ Associated k af
  where
	af = SFilePath (getTopFilePath f)

-- Does not remove any old association for a file, but less expensive
-- than addAssociatedFile. Calling dropAllAssociatedFiles first and then
-- this is an efficient way to update all associated files.
addAssociatedFileFast :: Key -> TopFilePath -> WriteHandle -> IO ()
addAssociatedFileFast k f = queueDb $ void $ insertUnique $ Associated k af
  where
	af = SFilePath (getTopFilePath f)

dropAllAssociatedFiles :: WriteHandle -> IO ()
dropAllAssociatedFiles = queueDb $
	deleteWhere ([] :: [Filter Associated])

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> ReadHandle -> IO [TopFilePath]
getAssociatedFiles k = readDb $ do
	l <- selectList [AssociatedKey ==. k] []
	return $ map (asTopFilePath . (\(SFilePath f) -> f) . associatedFile . entityVal) l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> ReadHandle -> IO [Key]
getAssociatedKey f = readDb $ do
	l <- selectList [AssociatedFile ==. af] []
	return $ map (associatedKey . entityVal) l
  where
	af = SFilePath (getTopFilePath f)

removeAssociatedFile :: Key -> TopFilePath -> WriteHandle -> IO ()
removeAssociatedFile k f = queueDb $
	deleteWhere [AssociatedKey ==. k, AssociatedFile ==. af]
  where
	af = SFilePath (getTopFilePath f)

addInodeCaches :: Key -> [InodeCache] -> WriteHandle -> IO ()
addInodeCaches k is = queueDb $
	forM_ is $ \i -> insertUnique $ Content k i 
		(inodeCacheToFileSize i)
		(inodeCacheToEpochTime i)

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: Key -> ReadHandle -> IO [InodeCache]
getInodeCaches k = readDb $ do
	l <- selectList [ContentKey ==. k] []
	return $ map (contentInodecache . entityVal) l

removeInodeCaches :: Key -> WriteHandle -> IO ()
removeInodeCaches k = queueDb $
	deleteWhere [ContentKey ==. k]

{- Check if the inode is known to be used for an annexed file. -}
isInodeKnown :: InodeCache -> SentinalStatus -> ReadHandle -> IO Bool
isInodeKnown i s = readDb (isJust <$> selectFirst q [])
  where
	q 
		| sentinalInodesChanged s =
			-- Note that this select is intentionally not
			-- indexed. Normally, the inodes have not changed,
			-- and it would be unncessary work to maintain
			-- indexes for the unusual case.
			[ ContentFilesize ==. inodeCacheToFileSize i
			, ContentMtime >=. tmin
			, ContentMtime <=. tmax
			]
		| otherwise = [ContentInodecache ==. i]
	(tmin, tmax) = inodeCacheEpochTimeRange i
