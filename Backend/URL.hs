{- git-annex "URL" backend -- keys whose content is available from urls.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.URL (
	backends,
	fromUrl
) where

import Common.Annex
import Types.Backend
import Types.Key
import Backend.Utilities

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend
	{ name = "URL"
	, getKey = const $ return Nothing
	, verifyKeyContent = Nothing
	, canUpgradeKey = Nothing
	, fastMigrate = Nothing
	-- The content of an url can change at any time, so URL keys are
	-- not stable.
	, isStableKey = const False
	}

{- Every unique url has a corresponding key. -}
fromUrl :: String -> Maybe Integer -> Key
fromUrl url size = stubKey
	{ keyName = genKeyName url
	, keyBackendName = "URL"
	, keySize = size
	}
