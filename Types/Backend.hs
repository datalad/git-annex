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

data BackendA a = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: FilePath -> a (Maybe Key),
	-- called during fsck to check a key, if the backend has its own checks
	fsckKey :: Maybe (Key -> FilePath -> a Bool)
}

instance Show (BackendA a) where
	show backend = "Backend { name =\"" ++ name backend ++ "\" }"

instance Eq (BackendA a) where
	a == b = name a == name b
