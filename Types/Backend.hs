{- git-annex key/value backend data type
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Backend where

import Types.Key
import Types.KeySource

data BackendA a = Backend
	{ name :: String
	, getKey :: KeySource -> a (Maybe Key) 
	, fsckKey :: Maybe (Key -> FilePath -> a Bool)
	, canUpgradeKey :: Maybe (Key -> Bool)
	}

instance Show (BackendA a) where
	show backend = "Backend { name =\"" ++ name backend ++ "\" }"

instance Eq (BackendA a) where
	a == b = name a == name b
