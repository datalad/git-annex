{- git-annex "URL" and "VURL" backends -- keys whose content is
 - available from urls.
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
backends = [backendURL, backendVURL]

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
	}

backendVURL :: Backend
backendVURL = Backend
	{ backendVariety = VURLKey
	, genKey = Nothing
	, verifyKeyContent = Nothing -- TODO
	, verifyKeyContentIncrementally = Nothing -- TODO
	, canUpgradeKey = Nothing
	, fastMigrate = Nothing
	-- Even if a hash is recorded on initial download from the web and
	-- is used to verify every subsequent transfer including other
	-- downloads from the web, in a split-brain situation there
	-- can be more than one hash and different versions of the content.
	-- So the content is not stable.
	, isStableKey = const False
	, isCryptographicallySecure = False 
	-- TODO it is when all recorded hashes are
	}

{- Every unique url has a corresponding key. -}
fromUrl :: String -> Maybe Integer -> Bool -> Key
fromUrl url size verifiable = mkKey $ \k -> k
	{ keyName = genKeyName url
	, keyVariety = if verifiable then VURLKey else URLKey
	, keySize = size
	}
