{- git-annex key backend data type
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Backend where

import Types.Key
import Types.KeySource

import Utility.Metered
import Utility.FileSystemEncoding

data BackendA a = Backend
	{ backendVariety :: KeyVariety
	, getKey :: Maybe (KeySource -> MeterUpdate -> a Key)
	-- Verifies the content of a key.
	, verifyKeyContent :: Maybe (Key -> FilePath -> a Bool)
	-- Checks if a key can be upgraded to a better form.
	, canUpgradeKey :: Maybe (Key -> Bool)
	-- Checks if there is a fast way to migrate a key to a different
	-- backend (ie, without re-hashing).
	, fastMigrate :: Maybe (Key -> BackendA a -> AssociatedFile -> a (Maybe Key))
	-- Checks if a key is known (or assumed) to always refer to the
	-- same data.
	, isStableKey :: Key -> Bool
	}

instance Show (BackendA a) where
	show backend = "Backend { name =\"" ++ decodeBS (formatKeyVariety (backendVariety backend)) ++ "\" }"

instance Eq (BackendA a) where
	a == b = backendVariety a == backendVariety b
