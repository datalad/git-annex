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

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend {
	name = "URL",
	getKey = const (return Nothing),
	fsckKey = Nothing
}

fromUrl :: String -> Maybe Integer -> Key
fromUrl url size = stubKey
	{ keyName = url
	, keyBackendName = "URL"
	, keySize = size
	}
