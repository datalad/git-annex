{- git-annex UUID type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.UUID where

-- A UUID is either an arbitrary opaque string, or UUID info may be missing.
data UUID = NoUUID | UUID String
	deriving (Eq, Ord)

instance Show UUID where
	show (UUID u) = u
	show NoUUID = ""

instance Read UUID where
	readsPrec _ s
		| null s = [(NoUUID, "")]
		| otherwise = [(UUID s, "")]
