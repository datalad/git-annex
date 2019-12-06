{- types for SQL databases
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Types (
	module Database.Types,
	Key,
	EpochTime,
	FileSize,
) where

import Database.Persist.Class hiding (Key)
import Database.Persist.Sql hiding (Key)
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as A
import System.PosixCompat.Types
import Data.Int
import Data.Text.Read
import Foreign.C.Types

import Key
import Utility.InodeCache
import Utility.FileSize
import Utility.FileSystemEncoding
import Git.Types
import Types.UUID
import Types.Import

instance PersistField Key where
	toPersistValue = toPersistValue . serializeKey'
	fromPersistValue b = fromPersistValue b >>= parse
	  where
		parse = either (Left . T.pack) Right . A.parseOnly keyParser

-- A key can contain arbitrarily encoded characters, so store in sqlite as a
-- blob to avoid encoding problems.
instance PersistFieldSql Key where
	sqlType _ = SqlBlob

instance PersistField InodeCache where
	toPersistValue = toPersistValue . showInodeCache 
	fromPersistValue b = fromPersistValue b >>= parse
	  where
		parse s = maybe
			(Left $ T.pack $ "bad serialized InodeCache "++ s)
			Right
			(readInodeCache s)

instance PersistFieldSql InodeCache where
	sqlType _ = SqlString

instance PersistField UUID where
	toPersistValue u = toPersistValue b
	  where
		b :: S.ByteString
		b = fromUUID u
	fromPersistValue v = toUUID <$> go
	  where
	 	go :: Either T.Text S.ByteString
		go = fromPersistValue v

instance PersistFieldSql UUID where
	sqlType _ = SqlBlob

instance PersistField ContentIdentifier where
	toPersistValue (ContentIdentifier b) = toPersistValue b
	fromPersistValue v = ContentIdentifier <$> go
	  where
	 	go :: Either T.Text S.ByteString
		go = fromPersistValue v

instance PersistFieldSql ContentIdentifier where
	sqlType _ = SqlBlob

-- A serialized FilePath. Stored as a ByteString to avoid encoding problems.
newtype SFilePath = SFilePath S.ByteString
	deriving (Eq, Show)

toSFilePath :: FilePath -> SFilePath
toSFilePath = SFilePath . encodeBS

fromSFilePath :: SFilePath -> FilePath
fromSFilePath (SFilePath b) = decodeBS b

instance PersistField  SFilePath where
	toPersistValue (SFilePath b) = toPersistValue b
	fromPersistValue v = SFilePath <$> fromPersistValue v

instance PersistFieldSql SFilePath where
	sqlType _ = SqlBlob

-- A serialized git Sha
newtype SSha = SSha String
	deriving (Eq, Show)

toSSha :: Sha -> SSha
toSSha (Ref s) = SSha s

fromSSha :: SSha -> Ref
fromSSha (SSha s) = Ref s

instance PersistField SSha where
	toPersistValue (SSha b) = toPersistValue b
	fromPersistValue v = SSha <$> fromPersistValue v

instance PersistFieldSql SSha where
	sqlType _ = SqlString

-- A FileSize could be stored as an Int64, but some systems could
-- conceivably have a larger filesize, and no math is ever done with them
-- in sqlite, so store a string instead.
instance PersistField FileSize where
	toPersistValue = toPersistValue . show
	fromPersistValue v = fromPersistValue v >>= parse
	  where
		parse = either (Left . T.pack) (Right . fst) . decimal

instance PersistFieldSql FileSize where
	sqlType _ = SqlString

-- Store EpochTime as an Int64, to allow selecting values in a range.
instance PersistField EpochTime where
	toPersistValue (CTime t) = toPersistValue (fromIntegral t :: Int64)
	fromPersistValue v = CTime . fromIntegral <$> go
	  where
		go :: Either T.Text Int64
		go = fromPersistValue v

instance PersistFieldSql EpochTime where
	sqlType _ = SqlInt64
