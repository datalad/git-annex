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

-- A serialized Key
newtype SKey = SKey String
	deriving (Show, Read)

toSKey :: Key -> SKey
toSKey = SKey . key2file

fromSKey :: SKey -> Key
fromSKey (SKey s) = fromMaybe (error $ "bad serialied key " ++ s) (file2key s)

derivePersistField "SKey"
