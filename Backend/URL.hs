{- git-annex URL backend -- keys whose content is available from urls.
 -
 - Copyright 2011-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.URL (
	backends,
	fromUrl,
	otherUrlKey,
) where

import Annex.Common
import Types.Key
import Types.Backend
import Backend.Utilities
import Backend.VURL.Utilities (migrateFromURLToVURL)

backends :: [Backend]
backends = [backendURL]

backendURL :: Backend
backendURL = Backend
	{ backendVariety = URLKey
	, genKey = Nothing
	, verifyKeyContent = Nothing
	, verifyKeyContentIncrementally = Nothing
	, canUpgradeKey = Nothing
	, fastMigrate = Just migrateFromURLToVURL
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

{- From an URL key to a VURL key and vice-versa. -}
otherUrlKey :: Key -> Maybe Key
otherUrlKey k
	| fromKey keyVariety k == URLKey = Just $
		alterKey k $ \kd -> kd { keyVariety = VURLKey }
	| fromKey keyVariety k == VURLKey = Just $
		alterKey k $ \kd -> kd { keyVariety = URLKey }
	| otherwise = Nothing
