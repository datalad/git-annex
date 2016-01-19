{- types for SQL databases
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TemplateHaskell #-}

module Database.Types where

import Database.Persist.TH
import Data.Maybe

import Types.Key
import Utility.InodeCache

-- A serialized Key
newtype SKey = SKey String
	deriving (Show, Read)

toSKey :: Key -> SKey
toSKey = SKey . key2file

fromSKey :: SKey -> Key
fromSKey (SKey s) = fromMaybe (error $ "bad serialied Key " ++ s) (file2key s)

derivePersistField "SKey"

-- A Key index. More efficient than SKey, but its Read instance does not
-- work when it's used in any kind of complex data structure.
newtype IKey = IKey String

instance Read IKey where
	readsPrec _ s = [(IKey s, "")]

instance Show IKey where
	show (IKey s) = s

toIKey :: Key -> IKey
toIKey = IKey . key2file

fromIKey :: IKey -> Key
fromIKey (IKey s) = fromMaybe (error $ "bad serialied Key " ++ s) (file2key s)

derivePersistField "IKey"

-- A serialized InodeCache
newtype SInodeCache = I String
	deriving (Show, Read)

toSInodeCache :: InodeCache -> SInodeCache
toSInodeCache = I . showInodeCache

fromSInodeCache :: SInodeCache -> InodeCache
fromSInodeCache (I s) = fromMaybe (error $ "bad serialied InodeCache " ++ s) (readInodeCache s)

derivePersistField "SInodeCache"
