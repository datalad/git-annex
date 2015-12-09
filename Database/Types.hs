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

-- A serialized InodeCache
newtype SInodeCache = I String
	deriving (Show, Read)

toSInodeCache :: InodeCache -> SInodeCache
toSInodeCache = I . showInodeCache

fromSInodeCache :: SInodeCache -> InodeCache
fromSInodeCache (I s) = fromMaybe (error $ "bad serialied InodeCache " ++ s) (readInodeCache s)

derivePersistField "SInodeCache"
