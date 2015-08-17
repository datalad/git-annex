{- Adds readonly support to remotes.
 -
 - Copyright 2013, 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.ReadOnly
	( adjustReadOnly
	, readonlyStoreKey
	, readonlyStorer
	, readonlyRemoveKey
	) where

import Common.Annex
import Types.Remote
import Types.StoreRetrieve
import Utility.Metered

{- Adds support for read-only remotes, by replacing the
 - methods that write to a remote with dummies that fail.
 -
 - Note that disabling git pushes to remotes is not handled here.
 -}
adjustReadOnly :: Remote -> Remote
adjustReadOnly r
	| remoteAnnexReadOnly (gitconfig r) = r
		{ storeKey = readonlyStoreKey
		, removeKey = readonlyRemoveKey
		, repairRepo = Nothing
		}
	| otherwise = r

readonlyStoreKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
readonlyStoreKey _ _ _ = readonlyFail

readonlyRemoveKey :: Key -> Annex Bool
readonlyRemoveKey _ = readonlyFail

readonlyStorer :: Storer
readonlyStorer _ _ _ = readonlyFail

readonlyFail :: Annex Bool
readonlyFail = do
	warning "this remote is readonly"
	return False
