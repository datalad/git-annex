{- git-annex URL backend -- keys whose content is available from urls.
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.URL (
	backends,
	fromUrl
) where

import Annex.Common
import Types.Key
import Types.Backend
import Backend.Utilities

backends :: [Backend]
backends = [backendURL]

backendURL :: Backend
backendURL = Backend
	{ backendVariety = URLKey
	, genKey = Nothing
	, verifyKeyContent = Nothing
	, verifyKeyContentIncrementally = Nothing
	, canUpgradeKey = Nothing
	, fastMigrate = Nothing
	-- The content of an url can change at any time, so URL keys are
	-- not stable.
	, isStableKey = const False
	, isCryptographicallySecure = False
	, isCryptographicallySecureKey = const (pure False)
	}

{- Every unique url has a corresponding key. -}
fromUrl :: String -> Maybe Integer -> Bool -> Key
fromUrl url size verifiable = mkKey $ \k -> k
	{ keyName = genKeyName url
	, keyVariety = if verifiable then VURLKey else URLKey
	, keySize = size
	}
