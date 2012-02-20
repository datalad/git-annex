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

import Data.Hash.MD5

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
	{ keyName = key
	, keyBackendName = "URL"
	, keySize = size
	}
	where
		-- when it's not too long, use the url as the key name
		-- 256 is the absolute filename max, but use a shorter
		-- length because this is not the entire key filename.
		key
			| length url < 128 = url
			| otherwise = take 128 url ++ "-" ++ md5s (Str url)
