{- git-annex key/value backend data type
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Backend where

import Types.Key

data Backend a = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: FilePath -> a (Maybe Key),
	-- called during fsck to check a key
	fsckKey :: Key -> a Bool
}

instance Show (Backend a) where
	show backend = "Backend { name =\"" ++ name backend ++ "\" }"

instance Eq (Backend a) where
	a == b = name a == name b
