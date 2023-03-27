{- git-annex "URL" backend -- keys whose content is available from urls.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
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
backends = [backend]

backend :: Backend
backend = Backend
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
	}

{- Every unique url has a corresponding key. -}
fromUrl :: String -> Maybe Integer -> Key
fromUrl url size = mkKey $ \k -> k
	{ keyName = genKeyName url
	, keyVariety = URLKey
	, keySize = size
	}
