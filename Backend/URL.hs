{- git-annex "URL" backend -- keys whose content is available from urls.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
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
	, fsckKey = Nothing
	, canUpgradeKey = Nothing
	}

{- Every unique url has a corresponding key. -}
fromUrl :: String -> Maybe Integer -> Annex Key
fromUrl url size = do
	n <- genKeyName url
	return $ stubKey
		{ keyName = n
		, keyBackendName = "URL"
		, keySize = size
		}
