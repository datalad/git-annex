{- Sqlite database of information about Keys
 -
 - Copyright 2015-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if MIN_VERSION_persistent_template(2,8,0)
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module Database.Keys.SQL where

import Database.Types
import Database.Handle
import qualified Database.Queue as H
import Utility.InodeCache
import Git.FilePath

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import Control.Monad
import Data.Maybe

-- Note on indexes: KeyFileIndex etc are really uniqueness constraints,
-- which cause sqlite to automatically add indexes. So when adding indexes,
-- have to take care to only add ones that work as uniqueness constraints.
-- (Unfortunately persistent does not support indexes that are not
-- uniqueness constraints; https://github.com/yesodweb/persistent/issues/109)
--
-- To speed up queries for a key, there's KeyFileIndex, 
-- which makes there be a covering index for keys.
--
-- FileKeyIndex speeds up queries that include the file, since
-- it makes there be a covering index for files. Note that, despite the name, it is
-- used as a uniqueness constraint now.
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
	-- commit queue after 10000 changes
	checkcommit sz _lastcommittime = pure (sz > 10000)

-- Insert the associated file.
-- When the file was associated with a different key before,
-- update it to the new key.
addAssociatedFile :: Key -> TopFilePath -> WriteHandle -> IO ()
addAssociatedFile k f = queueDb $
	void $ upsertBy
		(FileKeyIndex af k)
		(Associated k af)
		[AssociatedFile =. af, AssociatedKey =. k]
  where
	af = SFilePath (getTopFilePath f)

-- Faster than addAssociatedFile, but only safe to use when the file
-- was not associated with a different key before, as it does not delete
-- any old key.
newAssociatedFile :: Key -> TopFilePath -> WriteHandle -> IO ()
newAssociatedFile k f = queueDb $
	insert_ $ Associated k af
  where
	af = SFilePath (getTopFilePath f)

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> ReadHandle -> IO [TopFilePath]
getAssociatedFiles k = readDb $ do
	l <- selectList [AssociatedKey ==. k] []
	return $ map (asTopFilePath . (\(SFilePath f) -> f) . associatedFile . entityVal) l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none.) -}
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

removeInodeCache :: InodeCache -> WriteHandle -> IO ()
removeInodeCache i = queueDb $ deleteWhere
	[ ContentInodecache ==. i
	]

{- Check if the inode is known to be used for an annexed file. -}
isInodeKnown :: InodeCache -> SentinalStatus -> ReadHandle -> IO Bool
isInodeKnown i s = readDb (isJust <$> selectFirst q [])
  where
	q 
		| sentinalInodesChanged s =
			-- Note that this select is intentionally not
			-- indexed. Normally, the inodes have not changed,
			-- and it would be unnecessary work to maintain
			-- indexes for the unusual case.
			[ ContentFilesize ==. inodeCacheToFileSize i
			, ContentMtime >=. tmin
			, ContentMtime <=. tmax
			]
		| otherwise = [ContentInodecache ==. i]
	(tmin, tmax) = inodeCacheEpochTimeRange i
