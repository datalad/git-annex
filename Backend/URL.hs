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

import AnnexCommon
import Types.Backend
import Types.Key

backends :: [Backend Annex]
backends = [backend]

backend :: Backend Annex
backend = Types.Backend.Backend {
	name = "URL",
	getKey = const (return Nothing),
	fsckKey = const (return True)
}

fromUrl :: String -> Key
fromUrl url = stubKey { keyName = url, keyBackendName = "URL" }
