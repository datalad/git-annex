{- types for SQL databases
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Types where

import Database.Persist.TH
import Database.Persist.Class hiding (Key)
import Database.Persist.Sql hiding (Key)
import Data.Maybe
import Data.Char
import qualified Data.ByteString as S
import qualified Data.Text as T
import Control.DeepSeq

import Utility.PartialPrelude
import Key
import Utility.InodeCache
import Git.Types (Ref(..))
import Types.UUID
import Types.Import

-- A serialized Key
newtype SKey = SKey String
	deriving (Show, Read)

toSKey :: Key -> SKey
toSKey = SKey . serializeKey

fromSKey :: SKey -> Key
fromSKey (SKey s) = fromMaybe (error $ "bad serialized Key " ++ s) (deserializeKey s)

derivePersistField "SKey"

-- A Key index. More efficient than SKey, but its Read instance does not
-- work when it's used in any kind of complex data structure.
newtype IKey = IKey String

instance NFData IKey where
	rnf (IKey s) = rnf s

instance Read IKey where
	readsPrec _ s = [(IKey s, "")]

instance Show IKey where
	show (IKey s) = s

toIKey :: Key -> IKey
toIKey = IKey . serializeKey

fromIKey :: IKey -> Key
fromIKey (IKey s) = fromMaybe (error $ "bad serialized Key " ++ s) (deserializeKey s)

derivePersistField "IKey"

-- A serialized InodeCache
newtype SInodeCache = I String
	deriving (Show, Read)

toSInodeCache :: InodeCache -> SInodeCache
toSInodeCache = I . showInodeCache

fromSInodeCache :: SInodeCache -> InodeCache
fromSInodeCache (I s) = fromMaybe (error $ "bad serialized InodeCache " ++ s) (readInodeCache s)

derivePersistField "SInodeCache"

-- A serialized FilePath.
--
-- Not all unicode characters round-trip through sqlite. In particular,
-- surrigate code points do not. So, escape the FilePath. But, only when
-- it contains such characters.
newtype SFilePath = SFilePath String

-- Note that Read instance does not work when used in any kind of complex
-- data structure.
instance Read SFilePath where
	readsPrec _ s = [(SFilePath s, "")]

instance Show SFilePath where
	show (SFilePath s) = s

toSFilePath :: FilePath -> SFilePath
toSFilePath s@('"':_) = SFilePath (show s)
toSFilePath s
	| any needsescape s = SFilePath (show s)
	| otherwise = SFilePath s
  where
	needsescape c = case generalCategory c of
		Surrogate -> True
		PrivateUse -> True
		NotAssigned -> True
		_ -> False

fromSFilePath :: SFilePath -> FilePath
fromSFilePath (SFilePath s@('"':_)) =
	fromMaybe (error "bad serialized SFilePath " ++ s) (readish s)
fromSFilePath (SFilePath s) = s

derivePersistField "SFilePath"

-- A serialized Ref
newtype SRef = SRef Ref

-- Note that Read instance does not work when used in any kind of complex
-- data structure.
instance Read SRef where
	readsPrec _ s = [(SRef (Ref s), "")]

instance Show SRef where
	show (SRef (Ref s)) = s

derivePersistField "SRef"

toSRef :: Ref -> SRef
toSRef = SRef

fromSRef :: SRef -> Ref
fromSRef (SRef r) = r

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
