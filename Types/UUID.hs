{- git-annex UUID type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.UUID where

-- A UUID is either an arbitrary opaque string, or UUID info may be missing.
data UUID = NoUUID | UUID String
	deriving (Eq, Ord, Show)

fromUUID :: UUID -> String
fromUUID (UUID u) = u
fromUUID NoUUID = ""

toUUID :: String -> UUID
toUUID [] = NoUUID
toUUID s = UUID s
