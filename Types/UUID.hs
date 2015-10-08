{- git-annex UUID type
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types.UUID where

import qualified Data.Map as M
import qualified Data.UUID as U
import Data.Maybe

-- A UUID is either an arbitrary opaque string, or UUID info may be missing.
data UUID = NoUUID | UUID String
	deriving (Eq, Ord, Show, Read)

fromUUID :: UUID -> String
fromUUID (UUID u) = u
fromUUID NoUUID = ""

class ToUUID a where
	toUUID :: a -> UUID

instance ToUUID String where
	toUUID [] = NoUUID
	toUUID s = UUID s

isUUID :: String -> Bool
isUUID = isJust . U.fromString

type UUIDMap = M.Map UUID String
